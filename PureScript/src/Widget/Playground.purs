module Widget.Playground where

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
import Riverdragon.Dragon.Bones ((.$$), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (hatching, sourceCode, tabSwitcher)
import Riverdragon.River (Lake, dam, makeLake)
import Riverdragon.Roar.Live (discardDecls)
import Runtime (aSideChannel)
import Runtime as Runtime
import Runtime.Live (ImportsExprDecls(..), Status, compileInterface, fetchHighlight)
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
_sideChannel = aSideChannel (Proxy :: Proxy SideChannel) "Widget.Playground"


pipeline :: Lake String -> Lake Status
pipeline = Runtime.Live.pipeline
  { templateURL: "/assets/purs/Runtime.Playground/source.purs"
  , parseUser: Right Runtime.Live.importsExprDecls
  , templating: \template (ImportsExprDecls imports parserExpr decls) ->
      Runtime.Live.renameModuleTo "Runtime.Playground.Temp" $
      Runtime.Live.overrideValue
        { nameSearch: CST.T.Ident "parser"
        , exprReplace: parserExpr
        } $
      Runtime.Live.addImports imports $
      Runtime.Live.addDecls decls $
      discardDecls decls template
  }

embed :: Lake String -> Dragon
embed incomingRaw = hatching \shell -> do
  incoming <- shell.store do incomingRaw
  pipelined <- shell.track do Runtime.Live.ofPipeline (pipeline incoming)
  gotRenderer <- shell.store $ makeLake \cb -> mempty <$ _sideChannel.installChannel cb
  codeURL <- Runtime.configurable "codeURL" "https://tryps.veritates.love/assets/purs"
  let
    sourceCodeOf moduleName = fetchHighlight (codeURL <> "/" <> moduleName <> "/source.purs")
  pure $ D.Fragment
    [ D.div [ D.style =:= "white-space: pre" ] $ D.Replacing $
        mempty <$ gotRenderer <|> dam pipelined.compileStatus
    , pipelined.asScript >@ \latestScript -> fold
      [ gotRenderer >@ \renderer -> D.div [ D.Self =:= renderer.renderToEl ] mempty
      , latestScript
      ]
    , tabSwitcher Nothing
      [ "Template" /\ sourceCodeOf "Runtime.Playground"
      , "Templated" /\ pipelined.highlighted
      , "Compiled" /\ sourceCode "JavaScript" [] (D.Text (dam pipelined.compiled))
      ]
    ]

widget :: Widget
widget _ = pure $ compileInterface "Riverdragon.Main.Live" embed $ tripleQuoted """
    pure mempty
  """
