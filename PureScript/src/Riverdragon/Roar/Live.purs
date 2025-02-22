module Riverdragon.Roar.Live where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Idiolect (tripleQuoted)
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderEl)
import Riverdragon.Dragon.Bones ((.$), (.$$), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (Shell, eggy, sourceCode, tabSwitcher)
import Riverdragon.River (Lake, River, createRiverStore, dam, makeLake)
import Riverdragon.River as River
import Riverdragon.Roar.Synth (installSynth, notesToNoises)
import Riverdragon.Roar.Types (Roar, toRoars)
import Riverdragon.Roar.Viz (oscilloscope, spectrogram)
import Riverdragon.Roar.Yawn (YawnM)
import Runtime (aSideChannel)
import Runtime as Runtime
import Runtime.Live (ImportsExprDecls(..), Status, fetchHighlight)
import Runtime.Live as Runtime.Live
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Widget (Widget)

type DragonVoice =
  Shell -> Effect
  { dragon :: Dragon
  , voice :: (Int -> River Unit -> YawnM { value :: Array (Lake (Array Roar)), leave :: Lake Unit })
  }

mainForRoar :: DragonVoice -> Effect Unit
mainForRoar mkDragonVoice = do
  _sideChannel.messageInABottle
    { renderToEl: renderEl <@> eggy \shell -> do
        { dragon, voice } <- mkDragonVoice shell
        { send: sendScopeParent, stream: scopeParent } <- shell.track $ createRiverStore Nothing
        synth <- shell.track $ installSynth \noteStream -> do
          -- This triggers synthesizers and shuts them down when they are released
          activeSynths <- notesToNoises {} noteStream $ const voice

          scopeEl1 <- oscilloscope { width: 1024, height: 512 } activeSynths -- debug
          scopeEl2 <- spectrogram { height: 512, width: 400 } activeSynths
          void $ liftEffect $ River.subscribe scopeParent \el -> do
            Node.appendChild (HTMLCanvasElement.toNode scopeEl1) (Element.toNode el)
            Node.appendChild (HTMLCanvasElement.toNode scopeEl2) (Element.toNode el)
          pure $ toRoars activeSynths

        pure $ D.Fragment
          [ synth.playPause
          , synth.midi
          , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
          , dragon
          ]
    }

type SideChannel =
  { renderToEl :: Element -> Effect (Effect Unit)
  }

_sideChannel ::
  { installChannel :: (SideChannel -> Effect Unit) -> Effect Unit
  , messageInABottle :: SideChannel -> Effect Unit
  }
_sideChannel = aSideChannel (Proxy :: Proxy SideChannel) "Riverdragon.Roar.Live"

declValueName :: CST.T.Declaration Void -> Maybe String
declValueName = case _ of
  CST.T.DeclSignature (CST.T.Labeled { label: CST.T.Name { name: CST.T.Ident name } }) -> Just name
  CST.T.DeclValue { name: CST.T.Name { name: CST.T.Ident name } } -> Just name
  _ -> Nothing

discardDecls :: Array (CST.T.Declaration Void) -> CST.T.Module Void -> CST.T.Module Void
discardDecls decls = Runtime.Live.filterDecls \decl ->
  not maybe false (Array.elem <@> Array.mapMaybe declValueName decls) (declValueName decl)

pipeline :: Lake String -> Lake Status
pipeline = Runtime.Live.pipeline
  { templateURL: "/assets/purs/Riverdragon.Roar.Sky/source.purs"
  , parseUser: Right Runtime.Live.importsExprDecls
  , templating: \template (ImportsExprDecls imports parserExpr decls) ->
      Runtime.Live.renameModuleTo "Riverdragon.Temp" $
      Runtime.Live.overrideValue
        { nameSearch: CST.T.Ident "dragonVoice"
        , exprReplace: parserExpr
        } $
      Runtime.Live.addImports imports $
      Runtime.Live.addDecls decls $
      discardDecls decls template
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
        -- , "Riverdragon.Dragon.Bones as D" /\ assetFrame "Riverdragon.Dragon.Bones/docs"
        -- , "Riverdragon.Dragon.Wings as Wings" /\ assetFrame "Riverdragon.Dragon.Bones/docs"
        -- , "Riverdragon.River as River" /\ assetFrame "Riverdragon.River/docs"
        -- , "Riverdragon.River.Bed as Bed" /\ assetFrame "Riverdragon.River.Bed/docs"
        -- , "Riverdragon.River.Beyond as Beyond" /\ assetFrame "Riverdragon.River.Bed/docs"
        ]
      , "Source Code" /\ tabSwitcher (Just "Riverdragon.Dragon.Bones as D")
        [ "Riverdragon.Dragon" /\ sourceCodeOf "Riverdragon.Dragon"
        -- , "Riverdragon.Dragon.Bones as D" /\ sourceCodeOf "Riverdragon.Dragon.Bones"
        -- , "Riverdragon.Dragon.Wings as Wings" /\ sourceCodeOf "Riverdragon.Dragon.Bones"
        -- , "Riverdragon.River as River" /\ sourceCodeOf "Riverdragon.River"
        -- , "Riverdragon.River.Bed as Bed" /\ sourceCodeOf "Riverdragon.River.Bed"
        -- , "Riverdragon.River.Beyond as Beyond" /\ sourceCodeOf "Riverdragon.River.Bed"
        ]
      , "Live Code" /\ tabSwitcher (Just "Template")
        [ "Template" /\ sourceCodeOf "Riverdragon.Roar.Sky"
        , "Templated" /\ pipelined.highlighted
        , "Compiled" /\ sourceCode "JavaScript" [] (D.Text (dam pipelined.compiled))
        ]
      ]
    ]

widget :: Widget
widget _ = pure $ eggy \shell -> do
  let df = tripleQuoted """
    \shell -> do
      let
        -- A gentle organ voice to start off with
        voice semitones release = do
          -- Calculate the frequency from the MIDI note number
          freq <- mtf semitones
          -- A Knob (control) that represents a simple ADSR envelope
          ramp <- pure $ toKnob
            { adsr:
              { attack: 0.001
              , decay: 0.2
              , sustain: 0.8
              , release: 0.1
              }
            -- It starts immediately and releases on the release event
            , release
            }
          -- Build up a sound as a collection of pure sine tones
          let
            waves =
              [ { freq: freq * 1.0, gain: 1.0, ramp: ramp }
              , { freq: freq * 2.005, gain: 0.5, ramp: ramp }
              , { freq: freq * 3.004, gain: 0.28, ramp: ramp }
              , { freq: freq * 2.005 * 2.005, gain: 0.05, ramp: ramp }
              , { freq: freq * 5.0, gain: 0.04, ramp: ramp }
              , { freq: freq * 3.004 * 2.0, gain: 0.04, ramp: ramp }
              , { freq: freq * 7.0, gain: 0.02, ramp: ramp }
              , { freq: freq * 8.0, gain: 0.02, ramp: ramp }
              , { freq: freq * 9.0, gain: 0.01, ramp: ramp }
              ]
              -- Correct each frequency for its apparent loudness
              <#> loudnessCorrection
          -- Turn the information above into actual waves
          overtones <- for waves \wave -> do
            -- Sine wave oscillator
            sine <- Y.osc { type: Sine, frequency: wave.freq, detune: 0 }
            -- The base volume of the overtone
            scaled <- Y.gain wave.gain sine
            -- ADSR envelope
            decaying <- Y.gain wave.ramp scaled
            -- Return the Roar
            pure decaying
          -- Gather and scale all of the overtones to be quieter
          output <- Y.gain 0.05 overtones
          -- Return the sound, and a leave stream which
          -- lets the synth know that the voice is finished
          pure { value: [ output ], leave: dam (delay (1000.0 # Milliseconds) release) }

        dragon =
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
      pure { voice, dragon }
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

