module Parser.Main.Live where

import Prelude

import Ansi.Codes as Ansi
import Control.Plus ((<|>))
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Dodo as Dodo
import Effect (Effect)
import Idiolect (intercalateMap, tripleQuoted, (<#?>))
import JSURI (encodeURIComponent)
import Parser.Comb.Comber (Comber, Printer, parse, printGrammarWsn)
import PureScript.CST.Parser as CST.Parser
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderId)
import Riverdragon.Dragon.Bones (($$), (.$), (.$$), (.$$~), (:.), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode, tabSwitcher)
import Riverdragon.River (Lake, createRiverStore, makeLake)
import Riverdragon.River.Beyond (counter)
import Runtime (aSideChannel)
import Runtime as Runtime
import Runtime.Live (Status(..), fetchHighlight, highlighting)
import Runtime.Live as Runtime.Live
import Type.Proxy (Proxy(..))
import Web.DOM.ElementId (ElementId(..))
import Widget (Widget)

color :: Ansi.Color -> String
color = case _ of
  Ansi.Blue -> "cornflowerblue"
  x -> String.toLower (show x)

colorful :: Ansi.Color -> String -> Dragon
colorful x t = D.span
  [ D.style =:= "color:" <> color x ]
  (D.text t)

cls :: String -> String -> Dragon
cls c v = D.span:.c.$$v

toDragon :: Printer Dragon
toDragon =
  { nonTerminal: cls "non-terminal"
  , literal: \v -> D.code.$$v
  , regex: colorful Ansi.Green
      <<< (fromMaybe <*> String.stripPrefix (String.Pattern "/^(?:"))
      <<< (fromMaybe <*> String.stripSuffix (String.Pattern ")/su"))
  , rule: colorful Ansi.Cyan <<< show
  , meta: case _ of
      " " -> D.text " "
      t -> cls "meta" t
  , lines: foldMap (D.div[])
  }

mainForParser :: Comber (Dodo.Doc Void) -> Effect Unit
mainForParser parser = do
  _ <- renderId (ElementId "grammar") (printGrammarWsn toDragon parser)
  _sideChannel.messageInABottle
    { parser: parse parser >>> case _ of
        Left err -> { success: false, result: err }
        Right doc -> { success: true, result: Dodo.print Dodo.plainText Dodo.twoSpaces doc }
    , grammar: \_ -> []
    }

type IntoGrammar m =
  { terminal :: String -> m
  , nonTerminal :: String -> m
  , regex :: String -> m
  , meta :: String -> m
  }

type SideChannel =
  { parser :: String -> { success :: Boolean, result :: String }
  , grammar :: forall m. Monoid m =>
    IntoGrammar m ->
    Array { name :: String, syntax :: m }
  }

_sideChannel ::
  { installChannel :: (SideChannel -> Effect Unit) -> Effect Unit
  , messageInABottle :: SideChannel -> Effect Unit
  }
_sideChannel = aSideChannel (Proxy :: Proxy SideChannel) "Parser.Main.Live"


pipeline :: Lake String -> Lake Status
pipeline = Runtime.Live.pipeline
  { templateURL: "/assets/purs/Parser.Parserlude/source.purs"
  , parseUser: Right CST.Parser.parseExpr
  , templating: \template parserExpr ->
      Runtime.Live.renameModuleTo "Parser.Temp" $
        Runtime.Live.overrideValue
          { nameSearch: CST.T.Ident "parser"
          , exprReplace: parserExpr
          } template
  }

embed :: Lake String -> Dragon
embed incomingRaw = eggy \shell -> do
  incoming <- shell.instStore do incomingRaw
  pipelined <- shell.instStore do pipeline incoming
  gotParser <- shell.instStore $ makeLake \cb -> mempty <$ _sideChannel.installChannel cb
  let
    preScroll = D.pre [ D.style =:= "overflow-x: auto" ]
    -- the browser does not want to eval the same JavaScript script tag again,
    -- so we prepend a unique int
    bundled = map (\(code /\ i) -> "/*"<>show i<>"*/" <> code) $
      counter $ pipelined <#?> case _ of
        Compiled result -> Just result
        _ -> Nothing
    status = mempty <$ gotParser <|> do
      pipelined <#> case _ of
        Fetching _ -> D.text "Fetching ..."
        ParseErrors inTemplate errors -> preScroll $ errors #
          intercalateMap (D.br[]) (D.text <<< Runtime.Live.printPositionedError)
        Compiling _ -> D.text "Compiling ..."
        CompileErrors errs -> preScroll $ errs #
          intercalateMap (D.br[]) D.text
        Compiled _ -> D.text "Loading ..."
        Crashed err -> preScroll $$ err
  templated <- shell.instStore $ pipelined # filterMap case _ of
    Compiling code -> Just code
    _ -> Nothing
  compiled <- shell.instStore $ pipelined # filterMap case _ of
    Compiled code -> Just code
    _ -> Nothing
  codeURL <- Runtime.configurable "codeURL" "https://tryps.veritates.love/assets/purs"
  let
    assetFrame asset = D.html_"iframe"
      [ D.attr "src" =:= codeURL <> "/" <> asset <> ".html"
      , D.stylish =:= D.smarts { height: "calc(max(400px, 70vh))" }
      ]
      mempty
    sourceCodeOf moduleName = fetchHighlight (codeURL <> "/" <> moduleName <> "/source.purs")
  pure $ D.Fragment
    [ D.div [ D.style =:= "white-space: pre" ] $ D.Replacing status
    , D.html_"style".$$ tripleQuoted """
        #grammar .meta { font-size: 0.85em; margin: 0 }
        #grammar .non-terminal { font-size: 0.9em; margin: 0 }
      """
    , bundled >@ \latestBundle -> fold
      [ D.div [ D.id =:= "grammar" ] mempty
      , gotParser >@ \parser -> D.Egg do
          { send: setValue, stream: getValue } <- createRiverStore Nothing
          pure $ { destroy: mempty, dragon: _ } $ D.Fragment
            [ sourceCode "In" :."sourceCode unicode".$
                D.textarea
                  [ D.onInputValue =:= setValue
                  , D.value =:= ""
                  , D.style =:= "height: 15vh"
                  ]
            , sourceCode "Out" :."sourceCode unicode".$$~
                _.result <<< parser.parser <$> getValue
            ]
      , D.script
          -- We need to set `type="module"` before `src` in Riverdragon, because
          -- it applies attributes in order!
          [ D.prop "type" =:= "module"
          , D.attr "src" =:= do
              fromMaybe "" $ encodeURIComponent latestBundle <#> \escaped ->
                "data:text/javascript;utf8," <> escaped
          ]
      ]
    , D.html_"h2".$$ "Help"
    , tabSwitcher (Just "Docs")
      [ "Docs" /\ tabSwitcher (Just "Parser.Comb.Comber")
        [ "Parser.Comb.Comber" /\ assetFrame "Parser.Comb.Comber/docs"
        , "Parser.Comb" /\ assetFrame "Parser.Comb/docs"
        , "Parser.Comb.Types" /\ assetFrame "Parser.Comb.Types/docs"
        , "Parser.Comb.Combinators" /\ assetFrame "Parser.Comb.Combinators/docs"
        , "Parser.Selective" /\ assetFrame "Parser.Selective/docs"
        , "Parser.Printer.Types" /\ assetFrame "Parser.Printer.Types/docs"
        , "Parser.Printer.Juxt" /\ assetFrame "Parser.Printer.Juxt/docs"
        ]
      , "Source Code" /\ tabSwitcher (Just "Parser.Comb.Combinators")
        [ "Parser.Comb.Comber" /\ sourceCodeOf "Parser.Comb.Comber"
        , "Parser.Comb.Types" /\ sourceCodeOf "Parser.Comb.Types"
        , "Parser.Comb.Combinators" /\ sourceCodeOf "Parser.Comb.Combinators"
        , "Parser.Comb.Run" /\ sourceCodeOf "Parser.Comb.Run"
        , "Parser.Algorithms" /\ sourceCodeOf "Parser.Algorithms"
        , "Parser.Lexing" /\ sourceCodeOf "Parser.Lexing"
        , "Parser.Printer.Types" /\ sourceCodeOf "Parser.Printer.Types"
        , "Parser.Printer.Juxt" /\ sourceCodeOf "Parser.Printer.Juxt"
        ]
      , "Live Code" /\ tabSwitcher (Just "Template")
        [ "Template" /\ sourceCodeOf "Parser.Parserlude"
        , "Templated" /\ highlighting templated
        , "Compiled" /\ sourceCode "JavaScript" [] (D.Text compiled)
        ]
      ]
    ]

widget :: Widget
widget _ = pure $ eggy \shell -> do
  let df = tripleQuoted """
    -- This parses JSON-style strings separated by whitespace
    -- (the parser needs a name to assign to the `many` parser)
    manySepBy "strings" ws string
      -- And pretty prints their values separated by new lines
      <#> map D.text >>> D.lines
  """
  { stream: valueSet, send: setValue } <- shell.track $ createRiverStore Nothing
  { stream: compiling, send: compileNow } <- shell.track $ createRiverStore Nothing
  lastValue <- shell.storeLast df valueSet
  pure $ D.Fragment
    [ sourceCode "PureScript" .$ D.textarea
        [ D.onInputValue =:= setValue
        , D.value =:= df
        , D.style =:= "height: 40vh"
        ]
    , D.div.$ D.buttonW "" "Compile!" (compileNow =<< lastValue)
    , embed compiling
    ]

