module Riverdragon.Main.Live where

import Prelude

import Control.Plus ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Idiolect (intercalateMap, tripleQuoted, (<#?>))
import JSURI (encodeURIComponent)
import PureScript.CST.Parser as CST.Parser
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderEl)
import Riverdragon.Dragon.Bones (($$), (.$), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode)
import Riverdragon.River (Lake, createRiverStore, makeLake)
import Riverdragon.River.Beyond (counter)
import Runtime (aSideChannel)
import Runtime.Live (Status(..))
import Runtime.Live as Runtime.Live
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Widget (Widget)

mainForDragon :: Dragon -> Effect Unit
mainForDragon dragon = do
  log "Loading"
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
  { templateURL: "/assets/ps/Riverdragon.Dragon.Nest/index.purs"
  , parseUser: Right CST.Parser.parseExpr
  , templating: \template parserExpr ->
      Runtime.Live.renameModuleTo "Riverdragon.Temp" $
        Runtime.Live.overrideValue
          { nameSearch: CST.T.Ident "dragon"
          , exprReplace: parserExpr
          } template
  }

embed :: Lake String -> Dragon
embed incomingRaw = eggy \shell -> do
  incoming <- shell.instStore do incomingRaw
  pipelined <- shell.instStore do pipeline incoming
  gotRenderer <- shell.instStore $ makeLake \cb -> mempty <$ _sideChannel.installChannel cb
  let
    preScroll = D.pre [ D.style =:= "overflow-x: auto" ]
    -- the browser does not want to eval the same JavaScript script tag again,
    -- so we prepend a unique int
    bundled = map (\(code /\ i) -> "/*"<>show i<>"*/" <> code) $
      counter $ pipelined <#?> case _ of
        Compiled result -> Just result
        _ -> Nothing
    status = mempty <$ gotRenderer <|> do
      pipelined <#> case _ of
        Fetching _ -> D.text "Fetching ..."
        ParseErrors inTemplate errors -> preScroll $ errors #
          intercalateMap (D.br[]) (D.text <<< Runtime.Live.printPositionedError)
        Compiling -> D.text "Compiling ..."
        CompileErrors errs -> preScroll $ errs #
          intercalateMap (D.br[]) D.text
        Compiled _ -> D.text "Loading ..."
        Crashed err -> preScroll $$ err
  pure $ D.Fragment
    [ D.div [ D.style =:= "white-space: pre" ] $ D.Replacing status
    , bundled >@ \latestBundle -> fold
      [ gotRenderer >@ \renderer -> D.div [ D.Self =:= renderer.renderToEl ] mempty
      , D.script
          -- We need to set `type="module"` before `src` in Riverdragon, because
          -- it applies attributes in order!
          [ D.prop "type" =:= "module"
          , D.attr "src" =:= do
              fromMaybe "" $ encodeURIComponent latestBundle <#> \escaped ->
                "data:text/javascript;utf8," <> escaped
          ]
      ]
    ]

widget :: Widget
widget _ = pure $ eggy \shell -> do
  let df = tripleQuoted """
    -- Center the output
    D.div :."centered".$
    -- A context where we can run some effects
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
              D.div.$$ "*baps u " <> show (n+1) <> "ce*"
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
        ]
    , D.div.$ D.buttonW "" "Compile!" (compileNow =<< lastValue)
    , embed compiling
    ]

