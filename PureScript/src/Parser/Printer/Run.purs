module Parser.Printer.Run where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Functor.Compose (Compose(..))
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, un)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Dodo as T
import Dodo.Ansi as Dodo.Ansi
import Dodo.Internal as T
import Parser.Comb as Comb
import Parser.Comb.Comber as Comber
import Parser.Comb.Dragon (renderParseError)
import Parser.Lexing as Lexing
import Parser.Printer.Types (Ann(..), Opts, PrinterParser(..), applyBoundary)
import Parser.Printer.Types as IO
import Riverdragon.Dragon (Dragon)
import Whitespace as WS

runPrinterParser :: forall i o.
  PrinterParser i o ->
  { print :: i -> Renditions
  , parse :: String -> Either String o
  , fullParse ::
      String ->
      Either
        { error :: Comber.FullParseError
        , string :: Lazy String
        , dragon :: Lazy Dragon
        }
        (Parsed o)
  , compiled :: Comber.Compiled (Parsed o)
  }
runPrinterParser (PrinterParser pp) =
  let
    print0 = pp.printer \w -> T.flexGroup $ T.text "(" <> w <> T.text ")"
    print = print0 >>> \print1 ->
      renditions (print1 Nothing >>> WS.renderDoc)
    fromError error =
      { error
      , string: defer \_ -> Comber.convertParseError error
      , dragon: defer \_ -> renderParseError error
      }
    fromParsed (IO.Parsed p) =
      { cst: renditions p.cst
      , ast: p.ast
      }
    Compose (Comber.Comber parser) = applyBoundary pp.parser
    conf :: Comber.Conf
    conf = { best: Lexing.bestRegexOrString, defaultSpace: one }
    compiled :: Comber.Compiled (Parsed o)
    compiled = Comb.compile Comber.topName (fromParsed <$> parser)
    parse0 = Comb.execute conf compiled
    fullParse = parse0 >>> bimap fromError identity
    parse = fullParse >>> bimap (_.string >>> force) (_.ast >>> force)
  in { print, parse, fullParse, compiled }

type Parsed o =
  { cst :: Renditions
  , ast :: Lazy o
  }

type Renditions =
  { doc :: Opts -> T.Doc Ann
  , text :: Maybe T.PrintOptions -> Opts -> String
  , ansi :: Maybe T.PrintOptions -> Opts -> String
  -- , html :: Maybe T.PrintOptions -> Opts -> String
  -- , dragon :: Maybe T.PrintOptions -> Opts -> Dragon
  }

renditions :: (Opts -> T.Doc Ann) -> Renditions
renditions print =
  { doc: print
  , text: \style -> print >>> T.print T.plainText (fromMaybe T.twoSpaces style)
  , ansi: \style -> print >>> map (unwrap >>> _.ansi) >>> unravel >>> T.print Dodo.Ansi.ansiGraphics (fromMaybe T.twoSpaces style)
  }

unravel :: forall ann. T.Doc (Array ann) -> T.Doc ann
unravel = case _ of
  T.Append l r -> T.Append (unravel l) (unravel r)
  T.Indent d -> T.Indent (unravel d)
  T.Align i d -> T.Align i (unravel d)
  T.Annotate anns d -> Array.foldr T.Annotate (unravel d) anns
  T.FlexSelect x y z -> T.FlexSelect (unravel x) (unravel y) (unravel z)
  T.FlexAlt l r -> T.FlexAlt (unravel l) (unravel r)
  T.WithPosition f -> T.WithPosition (f >>> unravel)
  T.Local f -> T.Local (f >>> map unravel)
  T.Text i s -> T.Text i s
  T.Break -> T.Break
  T.Empty -> T.Empty
