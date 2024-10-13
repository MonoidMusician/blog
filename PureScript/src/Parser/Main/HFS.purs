module Parser.Main.HFS where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either, hush, isLeft)
import Data.Filterable (filterMap)
import Data.Foldable (fold, foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object as FO
import Idiolect (intercalateMap)
import Parser.Comb.Dragon (renderParseError)
import Parser.Languages.HFS (HFList, HFS, IsPointed(..), OpMeta(..), RuntimeError(..), emptyEnv, hfsCount, hfsFromInt, hfsToTree, opMeta, showHFS, stacksInfo, stdlib)
import Parser.Languages.HFS as HFS
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones (($$), ($<), ($<>), ($~~), (.$), (.$$), (.$~~), (<:>), (<>$), (=:=), (>$), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode)
import Riverdragon.River (createRiver)
import Riverdragon.River.Beyond (debounce, dedup)
import Web.DOM (Element)
import Widget (Widget)

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

color :: String -> String -> Dragon
color clr v = D.span [ D.style =:= ("color:"<>clr) ] $$ v

half :: Dragon -> Dragon
half = D.span [ D.style =:= "opacity:0.6" ]

widget :: Widget
widget _ = pure $ eggy \shell -> do
  { stream: valueSet, send: setValue } <- shell.track createRiver
  done <- shell.inst $ pure (false /\ Right emptyEnv) <|>
    map HFS.parseAndRun' <$> debounce (100.0 # Milliseconds) (dedup valueSet)
  let
    wrapOutput content =
      D.div [ D.className =:= "sourceCode", D.style =:= "" ] $
        D.pre.$ D.code [ D.style =:= "margin: 0" ] $ content
    renderOutput = wrapOutput <<< foldMap \{ building, pointed, values } ->
      D.div.$~~
        [ values # foldMapWithIndex \i { value, dequeued } -> D.div.$~~
            [ case pointed of
                NotPointed -> D.text "  "
                Pointed -> color "#7f2db9" $ "." <> show i
                Current -> color "#7f2db9" $ "$" <> show i
            , D.text " "
            , (if dequeued then color "#ff6565" else span "dv") $ showHFS value
            ]
        , D.div.$ building # foldMap \x ->
            (\y -> "--- " $< span "dv" y >$ " ---") case x of
              Left hfs | hfs == mempty -> "{"
              Left hfs -> showHFS (hfsCount hfs) <> "#{"
              Right 0 -> "["
              Right n -> show n <> "#["
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
              , D.style =:= "height: 20vh"
              ]
      , D.div [ D.style =:= "flex: 0 0 50%; overflow: auto; font-size: 70%" ] $
          gateSuccess identity done >@
            either
              (either renderParseError \(RuntimeError _ s) -> D.text $ "Runtime error: " <> s)
              (renderOutput <<< stacksInfo)
          ]
      ]
  where
  -- parseAndRun =
  topOfStack = snd >>> hush >=> stacksInfo >>> NEA.head >>> _.values >>> Array.head >>> map _.value
  gateSuccess f = filterMap \(Tuple force a) -> case f a, force of
    Left _, false -> Nothing
    r, _ -> Just r
