module Parser.Main.HFS where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either, hush, isLeft)
import Data.Filterable (filterMap)
import Data.Foldable (fold, foldMap, oneOf)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, xdata, (!:=), (<:=>))
import Deku.Control (switcher, switcherFlipped, text_)
import Deku.Core (Domable, Nut, bussed, envy)
import Deku.DOM as D
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import FRP.Helpers (dedup, debounce)
import FRP.Memoize (memoBeh)
import Foreign.Object as FO
import Idiolect (intercalateMap, (>==>))
import Parser.Languages.HFS (HFList, HFS, IsPointed(..), OpMeta(..), RuntimeError(..), emptyEnv, hfsCount, hfsFromInt, hfsToTree, opMeta, showHFS, stacksInfo, stdlib)
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
  , "Parser.Main.HFS.ops" /\ widget_ops All
  , "Parser.Main.HFS.ops_binary" /\ widget_ops Binaries
  , "Parser.Main.HFS.ops_n_ary" /\ widget_ops NAries
  , "Parser.Main.HFS.stdlib" /\ widget_stdlib
  ]

foreign import renderHFS :: EffectFn2 HFList Element Unit

graphHFS :: HFS -> Element -> Effect Unit
graphHFS hfs el = runEffectFn2 renderHFS (hfsToTree hfs) el

data OpsGroup = All | Binaries | NAries

widget_ops :: OpsGroup -> Widget
widget_ops select _ = do
  pure $ SafeNut do
    let
      ops dflt toks l r =
        Array.cons dflt toks <#> \s -> l <> s <> r
      binaries = opMeta >>= case _ of
        Symm _ dflt toks _bigops doc -> pure $ Tuple [ops dflt toks "" ""] $ text_ doc
        Sided _ _ dflt toks doc -> pure $ Tuple [ops dflt toks "" ".", ops dflt toks "." ""] $ text_ doc
        Order _ l ls _ r rs doc -> pure $ Tuple [ops l ls "" "", ops r rs "" ""] $ text_ doc
      naries = opMeta >>= case _ of
        Symm _ dflt toks bigops doc -> pure $ Tuple [bigops, ops dflt toks "#" "", ops dflt toks "" "#"] $ fold
          [ text_ "N-ary version of "
          , code "op" dflt
          , text_ ": "
          , text_ doc
          ]
        Sided _ _ dflt toks doc -> pure $ Tuple [ops dflt toks "#" "", ops dflt toks "" "#"] $ fold
          [ text_ "N-ary version of "
          , code "op" ("." <> dflt)
          , text_ " and "
          , code "op" (dflt <> ".")
          , text_ ": "
          , text_ doc
          ]
        Order _ l ls _ r rs doc -> pure $ Tuple [ops l ls "#" "", ops r rs "#" ""] $ fold
          [ text_ "N-ary version of "
          , code "op" l
          , text_ " and "
          , code "op" r
          , text_ ": "
          , text_ doc
          ]
      style = "column-width: 250px; padding-left: 16px; padding-right: 16px"
      selected = case select of
        All -> binaries <> naries
        Binaries -> binaries
        NAries -> naries
    D.div (D.Class !:= "") $ pure $ D.dl (D.Style !:= style) $
      selected <#> \(Tuple op doc) ->
        D.div (D.Style !:= "break-inside: avoid")
          [ D.dt_ [ intercalateMap (text_ ", ") (code "op") (join op) ]
          , D.dd_ [ doc ]
          ]

widget_stdlib :: Widget
widget_stdlib _ = do
  pure $ SafeNut do
    D.div
      ( D.Class !:= "sourceCode unicode"
      <|> pure (xdata "lang" "HatStack")
      ) $
        pure $ D.pre_ $ pure $ D.code_ $ pure $ text_ $ String.trim stdlib

code :: String -> String -> Nut
code cls c = D.code (D.Class !:= cls) [ text_ c ]

span :: String -> String -> Nut
span cls c = D.span (D.Class !:= cls) [ text_ c ]

color :: String -> String -> Nut
color clr v = D.span (D.Style !:= ("color:"<>clr)) [ text_ v ]

half :: forall lock payload. Domable lock payload -> Domable lock payload
half = D.span (D.Style !:= "opacity:0.6") <<< pure

widget :: Widget
widget _ = do
  pure $ SafeNut do
    let
      wrapOutput content =
        D.div (D.Class !:= "sourceCode" <|> D.Style !:= "") $ pure $
          D.pre_ $ pure $ D.code (D.Style !:= "margin: 0") $ content
      renderOutput = wrapOutput <<< foldMap \{ building, pointed, values } ->
        Array.singleton $ D.div_
          [ values # foldMapWithIndex \i { value, dequeued } -> D.div_
              [ case pointed of
                  NotPointed -> text_ "  "
                  Pointed -> color "#7f2db9" $ "." <> show i
                  Current -> color "#7f2db9" $ "$" <> show i
              , text_ " "
              , (if dequeued then color "#ff6565" else span "dv") $ showHFS value
              ]
          , D.div_ $ pure $ building # foldMap \x ->
              (\y -> text_ "--- " <> span "dv" y <> text_ " ---") case x of
                Left hfs | hfs == mempty -> "{"
                Left hfs -> showHFS (hfsCount hfs) <> "#{"
                Right 0 -> "["
                Right n -> show n <> "#["
          ]
    bussed \setValue valueSet -> envy $
      memoBeh (map HFS.parseAndRun' <$> debounce (100.0 # Milliseconds) (dedup valueSet)) (Tuple false (Right emptyEnv)) \done ->
        D.div_
          [ D.div_ [ switcherFlipped (pure (hfsFromInt 157842) <|> dedup (filterMap topOfStack done)) \hfs -> D.div (D.Self !:= (graphHFS hfs)) [] ]
          -- , D.div_ $ switcher
          , D.div (D.Style !:= "display: flex; width: 100%")
            [ D.div
                ( D.Style !:= "flex: 0 0 50%; padding-right: 10px; box-sizing: border-box;"
                <|> D.Class <:=> (done <#> snd >>> isLeft >>> if _ then "sourceCode unicode invalid" else "sourceCode unicode")
                <|> pure (xdata "lang" "HatStack")
                ) $
                pure $ D.pre_ $ pure $ D.code_ $ pure $
                  flip D.textarea [] $ oneOf
                    [ D.OnInput !:= updateTA (setValue <<< Tuple false)
                    , D.OnChange !:= updateTA (setValue <<< Tuple true)
                    , D.Style !:= "height: 20vh"
                    ]
            , D.div (D.Style !:= "flex: 0 0 50%; overflow: auto; font-size: 70%")
                [ switcher (either (either renderParseError \(RuntimeError _ s) -> text_ $ "Runtime error: " <> s) (renderOutput <<< stacksInfo)) $
                    gateSuccess identity done
                ]
            ]
          ]
  where
  -- parseAndRun =
  topOfStack = snd >>> hush >=> stacksInfo >>> NEA.head >>> _.values >>> Array.head >>> map _.value
  gateSuccess f = filterMap \(Tuple force a) -> case f a, force of
    Left _, false -> Nothing
    r, _ -> Just r
  updateTA upd = cb $ compose void $
    (Event.target >=> HTMLTextArea.fromEventTarget) >==>
      (HTMLTextArea.value >=> upd)
