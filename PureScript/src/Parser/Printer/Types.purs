module Parser.Printer.Types where

import Prelude

import Ansi.Codes (GraphicsParam)
import Control.Alternative (guard)
import Control.Apply (lift3)
import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Functor.App (App(..))
import Data.Lens as O
import Data.Maybe (Maybe, fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, dimap, lcmap)
import Data.These (These(..), these)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Dodo as Dodo
import Parser.Comb as Comb
import Parser.Comb.Comber (Comber, lift2)
import Parser.Comb.Comber as Comber
import Parser.Printer.Juxt (class Awajuxt, class Conjuxt, class Disjuxt, conjuxt0, conjuxt2, conjuxtR, disjuxt2)

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

printerParser :: forall ann i o. (i -> Dodo.Doc ann) -> Comber o -> PrinterParser ann i o
printerParser printer parser = PrinterParser { printer, parser, parserPrinter: pure $ repro parser }

printerParser' :: forall ann t. (t -> Dodo.Doc ann) -> Comber t -> PrinterParser ann t t
printerParser' printer parser = PrinterParser { printer, parser, parserPrinter: pure $ parser <#> printer }

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
infixr 5 namedRec as #->

-- namedPrecRec :: forall ann i o. String -> (PrinterParser ann i o -> WithPrec Rational (PrinterParser ann i o)) -> PrinterParser ann i o
-- namedPrecRec name mk = Comber <<< Comb.namedPrecRec name <<< coerce

-- namedPrec :: forall ann i o. String -> WithPrec Rational (PrinterParser ann i o) -> PrinterParser ann i o
-- namedPrec name = Comber <<< Comb.namedPrec name <<< coerce

manySepBy :: forall ann i o. String -> PrinterParser ann Unit Unit -> PrinterParser ann i o -> PrinterParser ann (Array i) (Array o)
manySepBy n s p =
  _arr $
    -- idk, i just find it so annoying that the bad UX
    -- is also harder to write, it is just so pointless,
    -- have to do the `disjuxt2 conjuxt0 $ conjuxt2` dance
    -- twice, ugh, it is not even bad cuz it is easy
    disjuxt2 conjuxt0 $ conjuxt2 p $
      n #-> \more -> _arr $
        disjuxt2 conjuxt0 $ conjuxt2 (conjuxtR s p) more
  where
  _arr :: O.Iso (Array i) (Array o) (Unit \/ (i /\ Array i)) (Unit \/ (o /\ Array o))
  _arr = dimap
    do Array.uncons >>> maybe (Left unit) (Right <<< (Tuple <$> _.head <*> _.tail))
    do either (const []) (uncurry Array.cons)
  -- map Array.fromFoldable $
  --   pure Nil <|> lift2 Cons p do
  --     n #-> \more -> pure Nil <|> lift2 Cons (s *> p) more


