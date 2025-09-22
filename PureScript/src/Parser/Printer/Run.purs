module Parser.Printer.Run where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Functor.Compose (Compose(..))
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Dodo as T
import Dodo.Ansi as Dodo.Ansi
import Dodo.Internal as T.I
import Parser.Comb as Comb
import Parser.Comb.Comber as Comber
import Parser.Comb.Dragon (annDragon, renderParseError)
import Parser.Comb.Types (withCST'_)
import Parser.Lexing as Lexing
import Parser.Printer.Types (Ann, Opts, PrinterParser(..), applyBoundary)
import Parser.Printer.Types as IO
import Parser.Types (ICST(..))
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
  , compiled :: Comber.Compiled { tree :: Array Comber.CST', result :: IO.Parsed o }
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
    fromParsed { before, result: { tree, result: IO.Parsed p }, after } =
      -- FIXME: CST whitespace text
      { cst: renditions \opts -> T.text before <> p.cst opts <> T.text after
      , ast: p.ast
      , tree: [IAir before] <> tree <> [IAir after]
      }
    Compose (Comber.Comber parser) = applyBoundary pp.parser
    conf :: Comber.Conf
    conf = { best: Lexing.bestRegexOrString, defaultSpace: one }
    compiled :: Comber.Compiled _
    compiled = Comb.compile Comber.topName (withCST'_ (\_ tree f -> { tree, result: _ } <$> f unit) parser)
    parse0 = Comb.execute conf compiled
    fullParse = parse0 >>> bimap fromError fromParsed
    parse = fullParse >>> bimap (_.string >>> force) (_.ast >>> force)
  in { print, parse, fullParse, compiled }

type Parsed o =
  { cst :: Renditions
  , tree :: Array Comber.CST'
  , ast :: Lazy o
  }

type Renditions =
  { doc :: Opts -> T.Doc Ann
  , text :: Maybe T.PrintOptions -> Opts -> String
  , ansi :: Maybe T.PrintOptions -> Opts -> String
  -- , html :: Maybe T.PrintOptions -> Opts -> String
  , dragon :: Maybe T.PrintOptions -> Opts -> Dragon
  }

renditions :: (Opts -> T.Doc Ann) -> Renditions
renditions print =
  { doc: print
  , text: \style -> print >>> T.print T.plainText (fromMaybe T.twoSpaces style)
  , ansi: \style -> print >>> map (unwrap >>> _.ansi) >>> unravel >>> T.print Dodo.Ansi.ansiGraphics (fromMaybe T.twoSpaces style)
  , dragon: \style -> print >>> T.print annDragon (fromMaybe T.twoSpaces style)
  }

unravel :: forall ann. T.Doc (Array ann) -> T.Doc ann
unravel = case _ of
  T.I.Append l r -> T.I.Append (unravel l) (unravel r)
  T.I.Indent d -> T.I.Indent (unravel d)
  T.I.Align i d -> T.I.Align i (unravel d)
  T.I.Annotate anns d -> Array.foldr T.I.Annotate (unravel d) anns
  T.I.FlexSelect x y z -> T.I.FlexSelect (unravel x) (unravel y) (unravel z)
  T.I.FlexAlt l r -> T.I.FlexAlt (unravel l) (unravel r)
  T.I.WithPosition f -> T.I.WithPosition (f >>> unravel)
  T.I.Local f -> T.I.Local (f >>> map unravel)
  T.I.Text i s -> T.I.Text i s
  T.I.Break -> T.I.Break
  T.I.Empty -> T.I.Empty
