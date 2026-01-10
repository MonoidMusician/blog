module Parser.Main.HFS where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either, hush, isLeft)
import Data.Filterable (filterMap)
import Data.Foldable (fold, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as FO
import Idiolect (intercalateMap)
import Parser.Comb.Dragon (renderParseError)
import Parser.Languages.HFS (Base(..), HFList, HFS(..), IsPointed(..), OpMeta(..), RuntimeError(..), bnBin, bnDec, bnHex, bnOct, bnQua, emptyEnv, hfs, hfsCount, hfsFromInt, hfsToTree, opMeta, stacksInfo, stdlib)
import Parser.Languages.HFS as HFS
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones (text, ($$), ($~~), (.$), (.$$), (.$~~), (<:>), (=:=), (>$), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (sourceCode)
import Riverdragon.River (River, createRiver, stillRiver, store, subscribe, subscribeM)
import Riverdragon.River.Beyond (debounce, dedup)
import Web.DOM (Element)
import Web.HTML (window)
import Web.HTML.Window (scroll)
import Widget (Widget, autoAdaptInterface)

widgets :: FO.Object Widget
widgets = FO.fromFoldable
  [ "Parser.Main.HFS.demo" /\ widget
  , "Parser.Main.HFS.example" /\ widget_example
  , "Parser.Main.HFS.ops" /\ widget_ops All
  , "Parser.Main.HFS.ops_binary" /\ widget_ops Binaries
  , "Parser.Main.HFS.ops_n_ary" /\ widget_ops NAries
  , "Parser.Main.HFS.stdlib" /\ widget_stdlib
  ]

foreign import displayHFS :: EffectFn2 HFList Element Unit

graphHFS :: HFS -> Element -> Effect Unit
graphHFS hfs el = runEffectFn2 displayHFS (hfsToTree hfs) el

data OpsGroup = All | Binaries | NAries

widget_ops :: OpsGroup -> Widget
widget_ops select _ = do
  let
    ops dflt toks l r =
      Array.cons dflt toks <#> \s -> l <> s <> r
    binaries = opMeta >>= case _ of
      Symm _ dflt toks _bigops doc -> pure $ Tuple [ops dflt toks "" ""] $ D.text doc
      Sided _ _ dflt toks doc -> pure $ Tuple [ops dflt toks "" ".", ops dflt toks "." ""] $ D.text doc
      Order _ l ls _ r rs doc -> pure $ Tuple [ops l ls "" "", ops r rs "" ""] $ D.text doc
    naries = opMeta >>= case _ of
      Symm _ dflt toks bigops doc -> pure $ Tuple [bigops, ops dflt toks "#" "", ops dflt toks "" "#"] $ fold
        [ D.text "N-ary version of "
        , code "op" dflt
        , D.text ": "
        , D.text doc
        ]
      Sided _ _ dflt toks doc -> pure $ Tuple [ops dflt toks "#" "", ops dflt toks "" "#"] $ fold
        [ D.text "N-ary version of "
        , code "op" ("." <> dflt)
        , D.text " and "
        , code "op" (dflt <> ".")
        , D.text ": "
        , D.text doc
        ]
      Order _ l ls _ r rs doc -> pure $ Tuple [ops l ls "#" "", ops r rs "#" ""] $ fold
        [ D.text "N-ary version of "
        , code "op" l
        , D.text " and "
        , code "op" r
        , D.text ": "
        , D.text doc
        ]
    style = "column-width: 250px; padding-left: 16px; padding-right: 16px"
    selected = case select of
      All -> binaries <> naries
      Binaries -> binaries
      NAries -> naries
  pure $ D.div.$ D.dl [ D.style =:= style ] $~~
    selected <#> \(Tuple op doc) ->
      D.div [ D.style =:= "break-inside: avoid" ] $~~
        [ D.dt.$ intercalateMap (D.text ", ") (code "op") (join op)
        , D.dd.$ doc
        ]

widget_stdlib :: Widget
widget_stdlib _ = pure $ sourceCode "HatStack".$$ String.trim stdlib

code :: String -> String -> Dragon
code cls c = D.code [ D.className =:= cls ] $$ c

span :: String -> String -> Dragon
span cls c = D.span [ D.className =:= cls ] $$ c

color :: String -> Dragon -> Dragon
color clr v = D.span [ D.style =:= ("color:"<>clr) ] $ v

half :: Dragon -> Dragon
half = D.span [ D.style =:= "opacity:0.6" ]

renderHFS :: HFS -> Dragon
renderHFS (NumLike b n) = case b of
  Bin -> span "dv" $ bnBin n
  Dec -> span "dv" $ bnDec n
  Hex -> span "dv" $ bnHex n
  Oct -> span "dv" $ bnOct n
  Qua -> span "dv" $ bnQua n
renderHFS (SetLike _ members) | Set.isEmpty members = span "" "∅"
renderHFS (SetLike _ members) | Set.size members == 1, [member] <- Set.toUnfoldable members =
  span "cf" "{" <> renderHFS member <> span "cf" "}"
renderHFS (SetLike _ members) = (\m -> span "cf" "{ " <> m <> span "cf" " }") $
  intercalateMap (text ", ") renderHFS (Array.reverse (Set.toUnfoldable members))


widget :: Widget
widget { interface } = do
  { stream: valueSet, send: setValue } <- createRiver
  let receiveValue = (autoAdaptInterface @String (interface "hatstack-example")).receive
  subscribe receiveValue $ setValue <<< Tuple true
  parser <- HFS.mkParser
  { stream: done } <- store $ pure (false /\ Right emptyEnv) <|>
    (map <$> stillRiver parser <*> debounce (100.0 # Milliseconds) (dedup valueSet))
  let
    clear = [ D.style =:= "border: 0; padding: 0" ]
    hr =
      D.html_"hr" [ D.style =:= "width: 100%" ] mempty
    hr' =
      D.html_"hr" [ D.style =:= "width: 100%; transform: scaleX(-1)" ] mempty
    andHr x = D.div [ D.style =:= "display: flex" ] $~~ [ x, hr ]
    cells = D.tr clear <<< foldMapWithIndex \i ->
      D.td if i == 0 then [ D.style =:= "border: 0; width: 3em; padding: 0" ] else clear
    strikethrough = """
      text-decoration: underline rgb(255, 101, 101);
      text-underline-offset: -38%;
      text-decoration-skip-ink: none;
    """
    renderOutput segments = D.table [ D.style =:= "border: 0; width: 100%"] $~~
      [ D.html_"colgroup".$~~
        [ D.html_"col" [ D.attr "span" =:= 1, D.style =:= "width: 3em" ] mempty
        , D.html_"col" [ D.attr "span" =:= 1, D.style =:= "width: auto" ] mempty
        ]
      , segments # foldMap \{ building, pointed, values } -> fold
        [ D.tbody clear $~~ values # mapWithIndex \i { value, dequeued } -> cells
            [ case pointed of
                NotPointed -> mempty
                Pointed -> D.code.$ color "#7f2db9" $$ "." <> show i
                Current -> D.code.$ color "#7f2db9" $$ "$" <> show i
            , D.code (if dequeued then [ D.style =:= strikethrough ] else []) $
                renderHFS value
            ]
        , building # foldMap \x -> D.tbody clear $ cells $
            (\y -> [ hr', andHr (D.code.$y) ]) case x of
              Left pending | pending == mempty -> span "cf" "{"
              Left pending -> renderHFS (hfsCount pending) <> span "cf" "#{" <>
                foldMap (\z -> renderHFS z >$ ", ") (hfs pending)
              Right 0 -> span "cf" "["
              Right n -> span "dv" (show n) <> span "cf" "#["
        ]
      ]
  pure $ D.div.$~~
    [ D.div.$ (pure (hfsFromInt 157842) <|> dedup (filterMap topOfStack done)) >@
        \hfs -> D.div [ pure $ D.Self $ map mempty <<< graphHFS hfs ] mempty
    , D.div [ D.style =:= "display: flex; width: 100%" ] $~~
      [ sourceCode "HatStack"
          [ D.style =:= "flex: 0 0 50%; padding-right: 10px; box-sizing: border-box;"
          , D.className <:> done <#> snd >>> isLeft >>> if _ then "sourceCode hatstack invalid" else "sourceCode hatstack"
          ] $ D.textarea
              [ D.onInputValue =:= setValue <<< Tuple false
              , D.onChangeValue =:= setValue <<< Tuple true
              , D.value <:> receiveValue
              , D.style =:= "height: 20svh"
              , D.asCodeInput
              ]
      , D.div [ D.style =:= "flex: 0 0 50%; overflow: auto; font-size: 100%" ] $
          gateSuccess identity done >@
            either
              (either renderParseError \(RuntimeError _ s) -> D.text $ "Runtime error: " <> s)
              (renderOutput <<< stacksInfo)
          ]
      ]
  where
  topOfStack = snd >>> hush >=> stacksInfo >>> NEA.head >>> _.values >>> Array.head >>> map _.value
  gateSuccess f = filterMap \(Tuple force a) -> case f a, force of
    Left _, false -> Nothing
    r, _ -> Just r

widget_example :: Widget
widget_example { interface, text } = liftEffect do
  let push = (autoAdaptInterface (interface "hatstack-example")).send
  example <- (String.trim <$> text) <> pure "\n"
  pure $ D.buttonW "" "Try this example" do
    push example
    scroll 0 0 =<< window
