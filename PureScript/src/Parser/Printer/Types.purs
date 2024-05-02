module Parser.Printer.Types where

import Prelude

import Ansi.Codes (GraphicsParam)
import Control.Alternative (class Alt)
import Control.Plus (empty, (<|>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Functor.Compose (Compose(..))
import Data.Lens as Q
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, over, un, unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.These (These(..), these)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Dodo as Dodo
import Parser.Comb as Comb
import Parser.Comb.Comber (Comber, lift2)
import Parser.Comb.Comber as Comber
import Parser.Printer.Juxt (class Awajuxt, class Conjuxt, class Disjuxt, class GuideFlow, CaseTree(..), _Array, _NEA, casesSplit, cleaveCases, (!!!), (!>), (/!\), (<!), (\!/))
import Parser.Selective (casingOn, hoistCaseTree', secondCaseTree)
import Type.Proxy (Proxy(..))
import Whitespace (Boundary)

-- TODO: standard syntactic categories
newtype Ann = Ann
  { classes :: Array String
  , ansi :: Array GraphicsParam
  }
derive instance newtypeAnn :: Newtype Ann _
derive newtype instance semigroupAnn :: Semigroup Ann
derive newtype instance monoidAnn :: Monoid Ann

newAnn ::
  ( { ansi :: Array GraphicsParam
    , classes :: Array String
    } ->
    { ansi :: Array GraphicsParam
    , classes :: Array String
    }
  ) ->
  Ann
newAnn f = Ann (f mempty)

newtype PrinterParser i o = PrinterParser
  { printer :: i -> Dodo.Doc Ann
  , parser :: Compose Comber (Tuple (Dodo.Doc Ann)) o
  }
type PP io = PrinterParser io io
-- I use `H`/`h` for separators since it is a silent letter and has no other meaning
type H = PP Unit

derive instance newtypePrinterParser :: Newtype (PrinterParser i o) _
derive instance profunctorPrinterParser :: Profunctor PrinterParser
derive instance functorPrinterParser :: Functor (PrinterParser i)
instance semigroupPrinterParser :: Semigroup o => Semigroup (PrinterParser i o) where
  append = lift2 append
instance monoidPrinterParser :: Monoid o => Monoid (PrinterParser i o) where
  mempty = pure mempty

-- Constructors and optics

printerParser :: forall i o. (i -> Dodo.Doc Ann) -> Comber o -> PrinterParser i o
printerParser printer parser = PrinterParser { printer, parser: carbonCopy parser }

printerParser' :: forall t. (t -> Dodo.Doc Ann) -> Comber t -> PrinterParser t t
printerParser' printer parser = PrinterParser { printer, parser: Compose $ parser <#> \t -> printer t /\ t }

_printer :: forall i i' o. Q.Lens (PrinterParser i o) (PrinterParser i' o) (i -> Dodo.Doc Ann) (i' -> Dodo.Doc Ann)
_printer = _Newtype <<< prop (Proxy :: Proxy "printer")

_parser :: forall i o o'. Q.Lens (PrinterParser i o) (PrinterParser i o') (Comber (Dodo.Doc Ann /\ o)) (Comber (Dodo.Doc Ann /\ o'))
_parser = _Newtype <<< prop (Proxy :: Proxy "parser") <<< _Newtype

_doc :: forall i o. Q.Setter' (PrinterParser i o) (Dodo.Doc Ann)
_doc f (PrinterParser pp) = PrinterParser pp
  { printer = f <<< pp.printer, parser = over Compose (map (lmap f)) pp.parser }

-- Interesting instances

instance conjuxtPrinterParser :: Conjuxt PrinterParser where
  conjuxt0 = mempty
  conjuxt2 (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: lcmap fst ux.printer <> lcmap snd vy.printer
    , parser: lift2 (/\) ux.parser vy.parser
    }

instance disjuxtPrinterParser :: Disjuxt PrinterParser where
  disjuxt0 = PrinterParser { printer: absurd, parser: empty }
  disjuxt2 (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: either ux.printer vy.printer
    , parser: Left <$> ux.parser <|> Right <$> vy.parser
    }

instance awajuxtPrinterParser :: Awajuxt PrinterParser where
  awajuxt2 (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: these ux.printer vy.printer (\u v -> ux.printer u <> vy.printer v)
    , parser: Both <$> ux.parser <*> vy.parser <|> This <$> ux.parser <|> That <$> vy.parser
    }
  awajuxtSep2 (PrinterParser ux) (PrinterParser sep) (PrinterParser vy) = PrinterParser
    { printer: these ux.printer vy.printer (\u v -> ux.printer u <> sep.printer unit <> vy.printer v)
    , parser: Both <$> ux.parser <* sep.parser <*> vy.parser <|> This <$> ux.parser <|> That <$> vy.parser
    }

instance guideFlowPrinterParser :: GuideFlow PrinterParser where
  -- seljuxt (PrinterParser ux) (SelP vu (PrinterParser pv) cases) = PrinterParser
  --   { printer: lcmap vu ux.printer <> pv.printer
  --   , parserPrinter: empty
  --   , parser: casingOn ux.parser (hoistCaseTree (unwrap >>> _.parser) cases)
  --   }
  -- branchCases (PrinterParser pij) (CasesSplit wuv jxy pixuz piyvz) = PrinterParser
  --   { printer: wuv >>> ?help
  --   , parserPrinter: empty
  --   , parser: ?help (map jxy pij.parser)
  --   }
  branchCases (PrinterParser pij) cases = PrinterParser
    { printer: wi >>> pij.printer
    , parser: Compose $ join <$> casingOn (un Compose pij.parser) (secondCaseTree caseTree)
    }
    where
    Tuple wi caseTree = cleaveCases (TwoCases (casesSplit cases))
      <#> hoistCaseTree' (\(PrinterParser pp) -> un Compose pp.parser)

instance applyPrinterParser :: Apply (PrinterParser i) where
  apply (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: ux.printer <> vy.printer
    , parser: ux.parser <*> vy.parser
    }
instance applicativePrinterParser :: Applicative (PrinterParser i) where
  pure a = PrinterParser { printer: mempty, parser: pure a }
-- | Left-biased!
instance altPrinterParser :: Alt (PrinterParser i) where
  alt (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: ux.printer
    , parser: ux.parser <|> vy.parser
    }


-- | Reproduce the exact source string in the output
-- TODO: parse newlines for Dodo printer ... what about indentation, idk
carbonCopy :: forall o. Comber o -> Compose Comber (Tuple (Dodo.Doc Ann)) o
carbonCopy = Compose <<< map (lmap Dodo.text) <<< Comber.withSourceOf


namedRec :: forall i o. String -> (PrinterParser i o -> PrinterParser i o) -> PrinterParser i o
namedRec name mk =
  let
    pO /\ qO = Comb.namedRec' name \parserI ->
      let
        made = unwrap $ mk $ PrinterParser
          { printer: \i -> made.printer i
          , parser: Compose (Comber.Comber parserI)
          }
        Compose (Comber.Comber parserO) = made.parser
        printerO = made.printer
      in parserO /\ printerO
  in PrinterParser { printer: qO, parser: Compose (Comber.Comber pO) }

named :: forall i o. String -> PrinterParser i o -> PrinterParser i o
named name pp = namedRec name (const pp)

infixr 2 named as #:
infixr 8 namedRec as #->

-- namedPrecRec :: forall i o. String -> (PrinterParser i o -> WithPrec Rational (PrinterParser i o)) -> PrinterParser i o
-- namedPrecRec name mk = Comber <<< Comb.namedPrecRec name <<< coerce

-- namedPrec :: forall i o. String -> WithPrec Rational (PrinterParser i o) -> PrinterParser i o
-- namedPrec name = Comber <<< Comb.namedPrec name <<< coerce

manySepBy :: forall i o. String -> PrinterParser Unit Unit -> PrinterParser i o -> PrinterParser (Array i) (Array o)
manySepBy name s p =
  _Array
  !!! pure unit
  \!/ p /!\ many name (s !> p)

many :: forall i o. String -> PrinterParser i o -> PrinterParser (Array i) (Array o)
many name p = name #-> \more ->
  _Array
  !!! pure unit
  \!/ p /!\ more

many1 :: forall i o. String -> PrinterParser i o -> PrinterParser (NonEmptyArray i) (NonEmptyArray o)
many1 name p =
  _NEA
  !!! p
  /!\ many name p

many_ :: forall i o. SepBy H -> String -> PrinterParser i o -> PrinterParser (Array i) (Array o)
many_ seps name p =
  let { sep, base, before, after } = applySepBy seps in
  _Array
  !!! base
  \!/ before !> p /!\ many name (sep !> p) <! after

many1_ :: forall i o. SepBy1 H -> String -> PrinterParser i o -> PrinterParser (NonEmptyArray i) (NonEmptyArray o)
many1_ NoSep1 name p = many1 name p
many1_ (SepBy1 h before after) name p =
  _NEA
  !!! applySepPref h before
   !> p
  /!\ many name (h !> p)
  <!  applySepPref h after

data SepBy h
  = NoSep
  | SepBy h SepPref SepPref SepPref
  | BaseSepBy h h SepPref SepPref

derive instance functorSepBy :: Functor SepBy

data SepBy1 h
  = NoSep1
  | SepBy1 h SepPref SepPref

derive instance functorSepBy1 :: Functor SepBy1

data SepPref
  = Require
  | Prefer
  | Allow
  | None

applySepPref :: forall f. Alt f => Applicative f => f Unit -> SepPref -> f Unit
applySepPref s Require = s
applySepPref s Prefer = s <|> pure unit
applySepPref s Allow = pure unit <|> s
applySepPref _ None = pure unit

applySepBy ::
  forall f.
    Applicative f =>
    Alt f =>
  SepBy (f Unit) ->
  { after :: f Unit
  , base :: f Unit
  , before :: f Unit
  , sep :: f Unit
  }
applySepBy NoSep =
  { sep: pure unit
  , base: pure unit
  , before: pure unit
  , after: pure unit
  }
applySepBy (SepBy h base before after) =
  { sep: h
  , base: applySepPref h base
  , before: applySepPref h before
  , after: applySepPref h after
  }
applySepBy (BaseSepBy h base before after) =
  { sep: h
  , base: base
  , before: applySepPref h before
  , after: applySepPref h after
  }
