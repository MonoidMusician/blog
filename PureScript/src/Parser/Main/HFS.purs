module Parser.Main.HFS where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either, hush, isLeft)
import Data.Filterable (filterMap)
import Data.Foldable (fold, oneOf)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, xdata, (!:=), (<:=>))
import Deku.Control (switcher, switcherFlipped, text_)
import Deku.Core (Nut, bussed, envy)
import Deku.DOM as D
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import FRP.Event.Time (debounce)
import FRP.Helpers (dedup)
import FRP.Memoize (memoBeh)
import Foreign.Object as FO
import Idiolect (intercalateMap, (>==>))
import Parser.Languages.HFS (HFS, HFList, OpMeta(..), StackKind(..), hfsFromInt, hfsToTree, opMeta, showHFS)
import Parser.Languages.HFS as HFS
import Parser.Main.Comb (renderParseError)
import Web.DOM (Element)
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Widget (Widget)
import Widget.Types (SafeNut(..))

widgets :: FO.Object Widget
widgets = FO.fromFoldable
  [ "Parser.Main.HFS.demo" /\ widget
  , "Parser.Main.HFS.ops" /\ widgetOps
  ]

foreign import renderHFS :: EffectFn2 HFList Element Unit

graphHFS :: HFS -> Element -> Effect Unit
graphHFS hfs el = runEffectFn2 renderHFS (hfsToTree hfs) el


widgetOps :: Widget
widgetOps _ = do
  pure $ SafeNut do
    let
      binaries = opMeta >>= case _ of
        Symm _ s doc -> pure $ Tuple [s] $ text_ doc
        Sided _ _ s doc -> pure $ Tuple [s <> ".", "." <> s] $ text_ doc
        Order _ l _ r doc -> pure $ Tuple [l, r] $ text_ doc
      naries = opMeta >>= case _ of
        Symm _ s doc -> pure $ Tuple ["#" <> s, s <> "#"] $ fold
          [ text_ "N-ary version of "
          , code "op" s
          , text_ ": "
          , text_ doc
          ]
        Sided _ _ s doc -> pure $ Tuple ["#" <> s, s <> "#"] $ fold
          [ text_ "N-ary version of "
          , code "op" ("." <> s)
          , text_ " and "
          , code "op" (s <> ".")
          , text_ ": "
          , text_ doc
          ]
        Order _ _ _ _ _ -> []
      style = "column-width: 250px; padding-left: 16px; padding-right: 16px"
    D.div (D.Class !:= "") $ pure $ D.dl (D.Style !:= style) $
      (binaries <> naries) <#> \(Tuple op doc) ->
        D.div (D.Style !:= "break-inside: avoid")
          [ D.dt_ [ intercalateMap (text_ ", ") (code "op") op ]
          , D.dd_ [ doc ]
          ]

code :: String -> String -> Nut
code cls c = D.code (D.Class !:= cls) [ text_ c ]

widget :: Widget
widget _ = do
  pure $ SafeNut do
    let
      renderOutput x = D.pre_
        [ text_ $ (intercalateMap "\n---\n" (intercalateMap "\n" showHFS)) x ]
    bussed \setValue valueSet -> envy $
      memoBeh (map HFS.parseAndRun' <$> debounce (100.0 # Milliseconds) (dedup valueSet)) (Tuple false (Right (pure []))) \done ->
        D.div_
          [ D.div_ [ switcherFlipped (pure (hfsFromInt 157842) <|> dedup (filterMap topOfStack done)) \hfs -> D.div (D.Self !:= (graphHFS hfs)) [] ]
          , D.div (D.Style !:= "display: flex; width: 100%")
            [ D.div
                ( D.Style !:= "flex: 0 0 50%; padding-right: 10px; box-sizing: border-box;"
                <|> D.Class <:=> (done <#> snd >>> isLeft >>> if _ then "sourceCode unicode invalid" else "sourceCode unicode")
                <|> pure (xdata "lang" "HFS")
                ) $
                pure $ D.pre_ $ pure $ D.code_ $ pure $
                  flip D.textarea [] $ oneOf
                    [ D.OnInput !:= updateTA (setValue <<< Tuple false)
                    , D.OnChange !:= updateTA (setValue <<< Tuple true)
                    , D.Style !:= "height: 20vh"
                    ]
            , D.div (D.Style !:= "flex: 0 0 50%; overflow: auto; font-size: 70%")
                [ switcher (either renderParseError renderOutput) $ gateSuccess identity done ]
            ]
          ]
  where
  -- parseAndRun =
  topOfStack = snd >>> hush >=> NEA.head >>> Array.head
  gateSuccess f = filterMap \(Tuple force a) -> case f a, force of
    Left _, false -> Nothing
    r, _ -> Just r
  updateTA upd = cb $ compose void $
    (Event.target >=> HTMLTextArea.fromEventTarget) >==>
      (HTMLTextArea.value >=> upd)
