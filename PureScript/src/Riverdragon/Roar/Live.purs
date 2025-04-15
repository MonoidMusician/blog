module Riverdragon.Roar.Live where

import Prelude

import Control.Monad.Reader (ask)
import Control.Plus ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Idiolect (tripleQuoted)
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderEl)
import Riverdragon.Dragon.Bones ((.$$), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (Shell, hatching, sourceCode, tabSwitcher)
import Riverdragon.River (Lake, River, createRiverStore, dam, makeLake)
import Riverdragon.River as River
import Riverdragon.Roar.Score (ScoreM, ScoreLive)
import Riverdragon.Roar.Score as Y
import Riverdragon.Roar.Synth (installSynth, notesToNoises)
import Riverdragon.Roar.Types (Roar, toRoars)
import Riverdragon.Roar.Viz (oscilloscope, spectrogram)
import Runtime (aSideChannel)
import Runtime as Runtime
import Runtime.Live (ImportsExprDecls(..), Status, compileInterface, fetchHighlight)
import Runtime.Live as Runtime.Live
import Type.Proxy (Proxy(..))
import Web.Audio.Types (BiquadFilterType(..))
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Widget (Widget)

type DragonVoice =
  Shell -> ScoreLive -> Effect
  { dragon :: Dragon
  , voice :: (Int -> River Unit -> ScoreM { value :: Array (Lake (Array Roar)), leave :: Lake Unit })
  , synthSetup :: ScoreM Unit
  , synthParameters ::
    { pitch :: Number
    , temperament :: Array Number
    }
  }

mainForRoar :: DragonVoice -> Effect Unit
mainForRoar mkDragonVoice = do
  _sideChannel.messageInABottle
    { renderToEl: renderEl <@> hatching \shell -> do
        lazyDragon <- shell.track $ createRiverStore Nothing
        { send: sendScopeParent, stream: scopeParent } <- shell.track $ createRiverStore Nothing
        synth <- shell.track $ installSynth \noteStream -> do
          { iface } <- ask
          { dragon, voice, synthSetup, synthParameters } <- liftEffect $ mkDragonVoice shell iface

          liftEffect $ iface.temperament.send synthParameters.temperament
          liftEffect $ iface.pitch.send synthParameters.pitch

          synthSetup

          liftEffect $ lazyDragon.send dragon

          -- This triggers synthesizers and shuts them down when they are released
          activeSynths <- notesToNoises {} noteStream $ const voice

          antialiased <- activeSynths # Y.filter
            { type: Lowpass
            , frequency: 15000.0
            , detune: 0.0
            , "Q": 0.3
            , gain: unit
            }

          scopeEl1 <- oscilloscope { width: 1024, height: 512 } antialiased
          scopeEl2 <- spectrogram { height: 512, width: 400 } antialiased
          void $ liftEffect $ River.subscribe scopeParent \el -> do
            Node.appendChild (HTMLCanvasElement.toNode scopeEl1) (Element.toNode el)
            Node.appendChild (HTMLCanvasElement.toNode scopeEl2) (Element.toNode el)
          pure $ toRoars antialiased

        pure $ D.Fragment
          [ synth.playPause
          , synth.midi
          , D.div [] $ D.Replacing lazyDragon.stream
          , D.div [ D.Self =:= \el -> mempty <$ sendScopeParent el ] mempty
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
      Runtime.Live.renameModuleTo "Riverdragon.Roar.Live.Temp" $
      Runtime.Live.overrideValue
        { nameSearch: CST.T.Ident "dragonVoice"
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
widget _ = pure $
  compileInterface "Riverdragon.Roar.Live" embed $ tripleQuoted """
    synthParameters =
      { pitch: 441.0
      , temperament: temperaments.kirnbergerIII
      }

    \shell iface@{ pitch, temperament } -> do
      perfectOvertones <- valueInterface false
      let
        -- A gentle organ voice to start off with
        voice semitones release = do
          -- Calculate the frequency from the MIDI note number
          initialFreq <- mtf semitones
          dynamicFreq <- mtf_ semitones
          -- A Knob (control) that represents a simple ADSR envelope
          ramp <- pure $ toKnob
            { adsr:
              { attack: 0.08 -- seconds
              , decay: 0.2 -- seconds
              , sustain: 0.8 -- gain
              , release: 0.08 -- seconds
              }
            -- It starts immediately and releases on the release event
            , release
            }
          let
            -- Scale a number over the range of the instrument
            ranging from to = S.linmap
              (S.Interval 20.0 83.0)
              (S.Interval from to)
              (Int.toNumber semitones)
            -- Build up a sound as a collection of pure sine tones
            waves =
              [ { overtone: 1.0, gain: ranging 0.5 1.0, ramp: ramp }
              , { overtone: 2.005, gain: ranging 1.0 0.5, ramp: ramp }
              , { overtone: 3.004, gain: 0.28, ramp: ramp }
              , { overtone: 2.005 * 2.005, gain: ranging 0.10 0.05, ramp: ramp }
              , { overtone: 5.0, gain: ranging 0.18 0.02, ramp: ramp }
              , { overtone: 3.004 * 2.0, gain: ranging 0.18 0.02, ramp: ramp }
              , { overtone: 7.0, gain: ranging 0.12 0.005, ramp: ramp }
              , { overtone: 8.0, gain: ranging 0.10 0.005, ramp: ramp }
              , { overtone: 9.0, gain: ranging 0.08 0.00, ramp: ramp }
              ]
              -- Correct each frequency for its apparent loudness
              -- (using A-Weighting ... it is not the best)
              <#> \r -> r { gain = r.gain / aWeighting (initialFreq * r.overtone) }
          -- Turn the information above into actual waves
          overtones <- for (A.take 10 waves) \wave -> do
            let
              -- Use the perfectOvertones stream to react to changes
              -- determining whether to use numerically perfect
              -- overtones or slightly distorted ones
              -- (try perfect intervals!)
              waveFreq = ado
                perfect <- perfectOvertones.loopback
                freq <- dynamicFreq
                in freq * if perfect
                  then Number.round wave.overtone
                  else wave.overtone
            -- Create a sine wave oscillator
            sine <- Y.osc { type: Sine, frequency: waveFreq, detune: 0 }
            -- Set the base volume of the overtone
            scaled <- Y.gain wave.gain sine
            -- Stack the ADSR envelope on top
            shaped <- Y.gain wave.ramp scaled
            -- Return the Roar
            pure (shaped :: Roar)
          -- Gather all of the overtones and quiet them down
          output <- Y.gain (ranging 0.35 0.10) overtones
          -- Return the sound, and a leave stream which
          -- lets the synth know that the voice is finished
          pure { value: [ output ], leave: dam (delay (1000.0 # Milliseconds) release) }

        dragon = D.Fragment
          -- Buttons to change parameters
          [ Wings.pushButtonRadio perfectOvertones
            [ true /\ D.text "Perfect overtones"
            , false /\ D.text "Imperfect overtones"
            ]
          , Wings.pushButtonRadio pitch
            [ 415.0 /\ D.text "A415"
            , 440.0 /\ D.text "A440"
            , 441.0 /\ D.text "A441"
            ]
          , Wings.pushButtonRadio temperament
            [ temperaments.equal /\ D.text "Equal Temperament"
            , temperaments.kirnbergerIII /\ D.text "Kirnberger III"
            , temperaments.pythagorean /\ D.text "Pythagorean"
            ]
          ]
      pure { voice, dragon }
  """

