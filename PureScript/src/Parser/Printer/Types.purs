module Parser.Printer.Types where

import Prelude

import Ansi.Codes (GraphicsParam)
import Control.Alternative (class Alt, guard)
import Control.Apply (lift3)
import Control.Plus (empty, (<|>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), either)
import Data.Functor.App (App(..))
import Data.Maybe (Maybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.These (These(..), these)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Dodo as Dodo
import Parser.Comb as Comb
import Parser.Comb.Comber (Comber, lift2)
import Parser.Comb.Comber as Comber
import Parser.Printer.Juxt (class Awajuxt, class Conjuxt, class Disjuxt, class GuideFlow, CaseTree(..), _Array, _NEA, casesSplit, cleaveCases, (!!!), (!>), (/!\), (<!), (\!/))
import Parser.Selective (casingOn, hoistCaseTree)

-- TODO: standard syntactic categories
newtype Ann = Ann
  { classes :: Array String
  , ansi :: Array GraphicsParam
  }
derive instance newtypeAnn :: Newtype Ann _
derive newtype instance semigroupAnn :: Semigroup Ann
derive newtype instance monoidAnn :: Monoid Ann

newtype PrinterParser ann i o = PrinterParser
  { printer :: i -> Dodo.Doc ann
  , parserPrinter :: App Maybe (Comber (Dodo.Doc ann))
  , parser :: Comber o
  }
type PP io = PrinterParser Ann io io
-- I use `H`/`h` for separators since it is a silent letter and has no other meaning
type H = PP Unit

derive instance newtypePrinterParser :: Newtype (PrinterParser ann i o) _
derive instance profunctorPrinterParser :: Profunctor (PrinterParser ann)
derive instance functorPrinterParser :: Functor (PrinterParser ann i)
derive newtype instance semigroupPrinterParser :: Semigroup o => Semigroup (PrinterParser ann i o)
derive newtype instance monoidPrinterParser :: Monoid o => Monoid (PrinterParser ann i o)

printerParser :: forall ann i o. (i -> Dodo.Doc ann) -> Comber o -> PrinterParser ann i o
printerParser printer parser = PrinterParser { printer, parser, parserPrinter: pure $ repro parser }

printerParser' :: forall ann t. (t -> Dodo.Doc ann) -> Comber t -> PrinterParser ann t t
printerParser' printer parser = PrinterParser { printer, parser, parserPrinter: pure $ parser <#> printer }

instance conjuxtPrinterParser :: Conjuxt (PrinterParser ann) where
  conjuxt0 = mempty
  conjuxt2 (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: lcmap fst ux.printer <> lcmap snd vy.printer
    , parserPrinter: ux.parserPrinter <> vy.parserPrinter
    , parser: lift2 (/\) ux.parser vy.parser
    }

instance disjuxtPrinterParser :: Disjuxt (PrinterParser ann) where
  disjuxt0 = PrinterParser { printer: absurd, parserPrinter: pure empty, parser: empty }
  disjuxt2 (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: either ux.printer vy.printer
    , parserPrinter: lift2 (<|>) ux.parserPrinter vy.parserPrinter
    , parser: Left <$> ux.parser <|> Right <$> vy.parser
    }

instance awajuxtPrinterParser :: Awajuxt (PrinterParser ann) where
  awajuxt2 (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: these ux.printer vy.printer (\u v -> ux.printer u <> vy.printer v)
    , parserPrinter: lift2 (\p q -> (p <> q) <|> p <|> q) ux.parserPrinter vy.parserPrinter
    , parser: Both <$> ux.parser <*> vy.parser <|> This <$> ux.parser <|> That <$> vy.parser
    }
  awajuxtSep2 (PrinterParser ux) (PrinterParser sep) (PrinterParser vy) = PrinterParser
    { printer: these ux.printer vy.printer (\u v -> ux.printer u <> sep.printer unit <> vy.printer v)
    , parserPrinter: lift3 (\p b q -> (p <> b <> q) <|> p <|> q) ux.parserPrinter sep.parserPrinter vy.parserPrinter
    , parser: Both <$> ux.parser <* sep.parser <*> vy.parser <|> This <$> ux.parser <|> That <$> vy.parser
    }

instance guideFlowPrinterParser :: GuideFlow (PrinterParser ann) where
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
    , parserPrinter: empty
    , parser: casingOn pij.parser caseTree
    }
    where
    Tuple wi caseTree = cleaveCases (TwoCases (casesSplit cases))
      <#> hoistCaseTree (\(PrinterParser pp) -> pp.parser)

instance applyPrinterParser :: Apply (PrinterParser ann i) where
  apply (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: ux.printer <> vy.printer
    , parserPrinter: ux.parserPrinter <> vy.parserPrinter
    , parser: ux.parser <*> vy.parser
    }
instance applicativePrinterParser :: Applicative (PrinterParser ann i) where
  pure a = PrinterParser { printer: mempty, parserPrinter: mempty, parser: pure a }
-- | Left-biased!
instance altPrinterParser :: Alt (PrinterParser ann i) where
  alt (PrinterParser ux) (PrinterParser vy) = PrinterParser
    { printer: ux.printer
    , parserPrinter: lift2 (<|>) ux.parserPrinter vy.parserPrinter
    , parser: ux.parser <|> vy.parser
    }


-- | Reproduce the exact source string in the output
-- TODO: parse newlines for Dodo printer ... what about indentation, idk
repro :: forall o ann. Comber o -> Comber (Dodo.Doc ann)
repro = map Dodo.text <<< Comber.sourceOf


namedRec :: forall ann i o. String -> (PrinterParser ann i o -> PrinterParser ann i o) -> PrinterParser ann i o
namedRec name mk =
  let
    pO /\ ppO /\ qO = Comb.namedRec' name \parserI ->
      let
        ppO /\ pO /\ hasCST /\ qO = Comb.namedRec' ("__" <> name) \parserPrinterI ->
          let
            made = unwrap $ mk $ PrinterParser
              { printer: \i -> made.printer i
              , parserPrinter: pure $ Comber.Comber parserPrinterI
              , parser: Comber.Comber parserI
              }
            App parserPrinterO = made.parserPrinter
            Comber.Comber parserO = made.parser
            printerO = made.printer
          in maybe empty unwrap parserPrinterO /\ parserO /\ isJust parserPrinterO /\ printerO
      in pO /\ (ppO <$ guard hasCST) /\ qO
  in PrinterParser { printer: qO, parserPrinter: Comber.Comber <$> ppO, parser: Comber.Comber pO }

named :: forall ann i o. String -> PrinterParser ann i o -> PrinterParser ann i o
named name (PrinterParser pp) = PrinterParser pp
  { parser = Comber.named name pp.parser
  , parserPrinter = Comber.named ("__" <> name) <$> pp.parserPrinter
  }

infixr 2 named as #:
infixr 8 namedRec as #->

-- namedPrecRec :: forall ann i o. String -> (PrinterParser ann i o -> WithPrec Rational (PrinterParser ann i o)) -> PrinterParser ann i o
-- namedPrecRec name mk = Comber <<< Comb.namedPrecRec name <<< coerce

-- namedPrec :: forall ann i o. String -> WithPrec Rational (PrinterParser ann i o) -> PrinterParser ann i o
-- namedPrec name = Comber <<< Comb.namedPrec name <<< coerce

manySepBy :: forall ann i o. String -> PrinterParser ann Unit Unit -> PrinterParser ann i o -> PrinterParser ann (Array i) (Array o)
manySepBy name s p =
  _Array
  !!! pure unit
  \!/ p /!\ many name (s !> p)

many :: forall ann i o. String -> PrinterParser ann i o -> PrinterParser ann (Array i) (Array o)
many name p = name #-> \more ->
  _Array
  !!! pure unit
  \!/ p /!\ more

many1 :: forall ann i o. String -> PrinterParser ann i o -> PrinterParser ann (NonEmptyArray i) (NonEmptyArray o)
many1 name p =
  _NEA
  !!! p
  /!\ many name p

many_ :: forall ann i o. SepBy (PrinterParser ann Unit Unit) -> String -> PrinterParser ann i o -> PrinterParser ann (Array i) (Array o)
many_ seps name p =
  let { sep, base, before, after } = applySepBy seps in
  _Array
  !!! base
  \!/ before !> p /!\ many name (sep !> p) <! after

many1_ :: forall ann i o. SepBy1 (PrinterParser ann Unit Unit) -> String -> PrinterParser ann i o -> PrinterParser ann (NonEmptyArray i) (NonEmptyArray o)
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
