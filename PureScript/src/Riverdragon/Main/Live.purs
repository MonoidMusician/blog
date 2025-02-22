module Riverdragon.Main.Live where

import Prelude

import Control.Plus ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Idiolect (tripleQuoted)
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderEl)
import Riverdragon.Dragon.Bones ((.$), (.$$), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode, tabSwitcher)
import Riverdragon.River (Lake, createRiverStore, dam, makeLake)
import Runtime (aSideChannel)
import Runtime as Runtime
import Runtime.Live (ImportsExprDecls(..), Status, fetchHighlight)
import Runtime.Live as Runtime.Live
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Widget (Widget)

mainForDragon :: Dragon -> Effect Unit
mainForDragon dragon = do
  _sideChannel.messageInABottle
    { renderToEl: renderEl <@> dragon
    }

type SideChannel =
  { renderToEl :: Element -> Effect (Effect Unit)
  }

_sideChannel ::
  { installChannel :: (SideChannel -> Effect Unit) -> Effect Unit
  , messageInABottle :: SideChannel -> Effect Unit
  }
_sideChannel = aSideChannel (Proxy :: Proxy SideChannel) "Riverdragon.Main.Live"


pipeline :: Lake String -> Lake Status
pipeline = Runtime.Live.pipeline
  { templateURL: "/assets/purs/Riverdragon.Dragon.Nest/source.purs"
  , parseUser: Right Runtime.Live.importsExprDecls
  , templating: \template (ImportsExprDecls imports parserExpr decls) ->
      Runtime.Live.renameModuleTo "Riverdragon.Temp" $
      Runtime.Live.overrideValue
        { nameSearch: CST.T.Ident "dragon"
        , exprReplace: parserExpr
        } $
      Runtime.Live.addImports imports $
      Runtime.Live.addDecls decls template
  }

embed :: Lake String -> Dragon
embed incomingRaw = eggy \shell -> do
  incoming <- shell.store do incomingRaw
  pipelined <- shell.track do Runtime.Live.ofPipeline (pipeline incoming)
  gotRenderer <- shell.store $ makeLake \cb -> mempty <$ _sideChannel.installChannel cb
  codeURL <- Runtime.configurable "codeURL" "https://tryps.veritates.love/assets/purs"
  let
    assetFrame asset = D.html_"iframe"
      [ D.attr "src" =:= codeURL <> "/" <> asset <> ".html"
      , D.stylish =:= D.smarts { height: "calc(max(400px, 70vh))" }
      ]
      mempty
    sourceCodeOf moduleName = fetchHighlight (codeURL <> "/" <> moduleName <> "/source.purs")
  pure $ D.Fragment
    [ D.div [ D.style =:= "white-space: pre" ] $ D.Replacing $
        mempty <$ gotRenderer <|> dam pipelined.compileStatus
    , pipelined.asScript >@ \latestScript -> fold
      [ gotRenderer >@ \renderer -> D.div [ D.Self =:= renderer.renderToEl ] mempty
      , latestScript
      ]
    , D.html_"h2".$$ "Help"
    , tabSwitcher (Just "Docs")
      [ "Docs" /\ tabSwitcher (Just "Riverdragon.River as River")
        [ "Riverdragon.Dragon" /\ assetFrame "Riverdragon.Dragon/docs"
        , "Riverdragon.Dragon.Bones as D" /\ assetFrame "Riverdragon.Dragon.Bones/docs"
        , "Riverdragon.Dragon.Wings as Wings" /\ assetFrame "Riverdragon.Dragon.Bones/docs"
        , "Riverdragon.River as River" /\ assetFrame "Riverdragon.River/docs"
        , "Riverdragon.River.Bed as Bed" /\ assetFrame "Riverdragon.River.Bed/docs"
        , "Riverdragon.River.Beyond as Beyond" /\ assetFrame "Riverdragon.River.Bed/docs"
        ]
      , "Source Code" /\ tabSwitcher (Just "Riverdragon.Dragon.Bones as D")
        [ "Riverdragon.Dragon" /\ sourceCodeOf "Riverdragon.Dragon"
        , "Riverdragon.Dragon.Bones as D" /\ sourceCodeOf "Riverdragon.Dragon.Bones"
        , "Riverdragon.Dragon.Wings as Wings" /\ sourceCodeOf "Riverdragon.Dragon.Bones"
        , "Riverdragon.River as River" /\ sourceCodeOf "Riverdragon.River"
        , "Riverdragon.River.Bed as Bed" /\ sourceCodeOf "Riverdragon.River.Bed"
        , "Riverdragon.River.Beyond as Beyond" /\ sourceCodeOf "Riverdragon.River.Bed"
        ]
      , "Live Code" /\ tabSwitcher (Just "Template")
        [ "Template" /\ sourceCodeOf "Riverdragon.Dragon.Nest"
        , "Templated" /\ pipelined.highlighted
        , "Compiled" /\ sourceCode "JavaScript" [] (D.Text (dam pipelined.compiled))
        ]
      ]
    ]

widget :: Widget
widget _ = pure $ eggy \shell -> do
  let df = tripleQuoted """
    -- Center the output
    D.div :."centered".$
      -- A context where we can run some effects
      -- (really: lifecycle management)
      eggy \shell -> do
        -- Create a stream that we will destroy when unloaded
        -- (not really necessary here, but good hygiene)
        { stream: clicked, send: onClick } <- shell.track $ createRiver
        -- Render a fixed list of items
        pure $ D.Fragment
          -- The first item is a button that triggers the above event
          [ D.button [ D.onClick =!= onClick unit ] $$ "bap"
          -- The second item is actually a list of items that disappear after 1 second
          , D.Appending $ Wings.vanishing (1000.0 # Milliseconds) $
              -- Each item appears after a click
              Beyond.counter clicked <#> \(Tuple _ n) ->
                -- And has this text, counting up
                D.div.$$ "*baps u " <> show (n+1) <>
                  (if n > 2 then "x" else "ce") <> "*"
          ]
  """
  { stream: valueSet, send: setValue } <- shell.track $ createRiverStore Nothing
  { stream: compiling, send: compileNow } <- shell.track $ createRiverStore Nothing
  lastValue <- shell.storeLast df valueSet
  pure $ D.Fragment
    [ sourceCode "PureScript" .$ D.textarea
        [ D.onInputValue =:= setValue
        , D.value =:= df
        , D.style =:= "height: 40vh"
        , D.asCodeInput
        ]
    , D.div.$ D.buttonW "" "Compile!" (compileNow =<< lastValue)
    , embed compiling
    ]

