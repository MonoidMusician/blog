module Widget.Unicode where

import Prelude

import Control.Apply (lift2)
import Control.Plus ((<|>))
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Codec.Argonaut as CA
import Data.Either (hush)
import Data.Enum (fromEnum)
import Data.Foldable (fold, foldMap, oneOf, oneOfMap, traverse_)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as CP
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Deku.Attribute (cb, (!:=), (<:=>))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, bussed)
import Deku.DOM as D
import FRP.Event (Event)
import FRP.Helpers (dedup)
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

widget :: Widget
widget { interface, attrs } = do
  example <- attrs "example"
  let
    initial = hush $ CA.decode CA.string example
    resetting = oneOfMap pure initial <|> do
      (adaptInterface CA.string (interface "unicode-example")).receive
  pure $ SafeNut do
    component resetting

only :: Array ~> Maybe
only [a] = Just a
only _ = Nothing

component :: forall lock payload. Event String -> Domable lock payload
component resetting =
  D.div (D.Style !:= "font-variant-numeric: lining-nums tabular-nums") $ pure $
    bussed \taCb taState' -> fold do
      let
        tf = if _ then "T" else "F"
        taState = dedup ({ value: _, start: 0, end: 0 } <$> (pure "" <|> resetting) <|> taState')
        taValue = taState <#> _.value
        taSelected = taState <#> \ta ->
          let r = CU.drop ta.start (CU.take ta.end ta.value)
          in if r == "" then ta.value else r
        taAllCPs = taValue <#> CP.toCodePointArray
        taCP = taState <#> \ta ->
          let r = CU.drop ta.start (CU.take ta.end ta.value)
          in if r == ""
            then Array.head $ CP.toCodePointArray $ CU.take 2 (CU.drop ta.start ta.value)
            else only (CP.toCodePointArray r)
        taCPs = taState <#> \ta ->
          let r = CU.drop ta.start (CU.take ta.end ta.value)
          in if r == ""
            then Array.take 1 $ CP.toCodePointArray $ CU.take 2 (CU.drop ta.start ta.value)
            else CP.toCodePointArray r
      [ D.div (D.Class !:= "sourceCode css") $ pure $ D.pre_ $ pure $ D.code_ $ pure $ flip D.textarea [] $ oneOf
        [ D.OnInput !:= updateTA taCb
        , D.OnMousedown !:= updateTA taCb
        , D.OnMouseup !:= updateTA taCb
        , D.OnKeydown !:= updateTA taCb
        , D.OnKeyup !:= updateTA taCb
        , D.OnKeypress !:= updateTA taCb
        , D.OnSelect !:= updateTA taCb
        , D.OnSelectionchange !:= updateTA taCb
        , D.Value <:=> resetting
        ]
      , D.table_
        [ renderInfos taSelected taValue
          [ Tuple "Code Point(s)" $ CP.length >>> show
          , Tuple "UTF-16 Code Unit(s)" $ CU.length >>> show
          ]
        , renderInfos taCPs taAllCPs
          [ Tuple "Alphas (letters)?" $ Array.all Unicode.isAlpha >>> tf
          , Tuple "AlphaNums (letters or numbers)?" $ Array.all Unicode.isAlphaNum >>> tf
          ]
        , renderInfos taCP taCP $
          [ Tuple "Code Point" $ foldMap $ fromEnum >>> Int.toStringAs hexadecimal >>> String.toUpper >>> append "U+"
          , Tuple "General Category" $ foldMap $ Unicode.generalCategory >>> foldMap show
          ]
        ]
      ]
  where
  updateTA upd = cb $
    (Event.target >=> HTMLTextArea.fromEventTarget) >>>
      traverse_ \ta -> do
        value <- HTMLTextArea.value ta
        start <- HTMLTextArea.selectionStart ta
        end <- HTMLTextArea.selectionEnd ta
        upd { value, start, end }
  renderInfos :: forall a. Event a -> Event a -> Array (Tuple String (a -> String)) -> _
  renderInfos x y infos = D.tbody_ $ infos <#> \(Tuple name calc) -> do
    let
      uv | unsafeRefEq x y = join Tuple <<< calc <$> x
      uv = lift2 Tuple (calc <$> x) (calc <$> y)
      code = D.span ((D.Style !:= "font-family: \"Fira Code\", Hasklig, monospace; font-weight: 300")) <<< pure <<< text_
      -- code = D.code_ <<< pure <<< text_
    D.tr_ $
      [ D.td_ $ pure $ text_ name <> text_ " "
      , D.td_ $ pure $ flip switcher uv \(Tuple u v) ->
          if u == v then code u else code u <> text_ " / " <> code v
      ]
