module Widget.Unicode where

import Prelude

import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Plus ((<|>))
import Data.Array as Array
import Data.CodePoint.Unicode (generalCatToUnicodeCat, isAlpha, isAlphaNum, isControl, isPrint, isPunctuation, isSpace, isSymbol)
import Data.CodePoint.Unicode as Unicode
import Data.Codec.Argonaut as CA
import Data.Either (hush)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (fold, foldMap, oneOf, oneOfMap, traverse_)
import Data.Int as Int
import Data.Int as Radix
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (CodePoint)
import Data.String as CP
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Deku.Attribute (cb, xdata, (!:=), (<:=>))
import Deku.Control (switcher, text_)
import Deku.Core (Domable, bussed)
import Deku.DOM as D
import Effect (Effect)
import FRP.Event (Event)
import FRP.Helpers (dedup)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

widget :: Widget
widget { interface, attrs } = do
  example <- attrs "unicode"
  let
    initial = hush $ CA.decode CA.string example
    stringInterface = adaptInterface CA.string (interface "unicode")
    resetting = oneOfMap pure initial <|> stringInterface.receive
  pure $ SafeNut do
    component stringInterface.send resetting

only :: Array ~> Maybe
only [a] = Just a
only _ = Nothing

uniprop ::
  forall s r.
    RowToList r (RL.Cons s (CodePoint -> Boolean) RL.Nil) =>
    Row.Cons s (CodePoint -> Boolean) () r =>
    IsSymbol s =>
  Record r -> Tuple String (Array CodePoint -> Display)
uniprop r = Tuple
  (reflectSymbol (Proxy :: Proxy s) # apply fromMaybe (String.stripPrefix (String.Pattern "is")) >>> append "are " >>> (_ <> "?"))
  (allsomenone (Record.get (Proxy :: Proxy s) r) >>> Text)

data Radix
  = Dec
  | Hex
  | Bin
derive instance Eq Radix

printRadix :: Maybe Radix -> Int -> String
printRadix Nothing = show
printRadix (Just Dec) = Int.toStringAs Radix.decimal
printRadix (Just Hex) = Int.toStringAs Radix.hexadecimal >>> String.toUpper >>> append "0x"
printRadix (Just Bin) = Int.toStringAs Radix.binary >>> append "0b"

data Display
  = Unicode CodePoint
  | Numeric String
  | Number (Maybe Radix) Int
  | Text String
  | Code String
  | Meta String
  | Many (Array Display)
derive instance Eq Display
instance Semigroup Display where
  append (Many as) (Many bs) = Many (as <> bs)
  append (Many []) d = d
  append d (Many []) = d
  append (Many as) b = Many (as <> [b])
  append a (Many bs) = Many ([a] <> bs)
  append a b = Many [a, b]
instance Monoid Display where
  mempty = Many []

display :: forall lock payload. Display -> Domable lock payload
display (Unicode cp) = display $ Meta "U+" <> Numeric do
  cp # fromEnum >>> Int.toStringAs Radix.hexadecimal >>> String.toUpper
display (Text txt) = text_ txt
display (Code c) = D.span (D.Class !:= "code") <<< pure <<< text_ $ c
display (Numeric n) = D.span (D.Class !:= "code numeric") <<< pure <<< text_ $ n
display (Number mr i) = display $ Numeric $ printRadix mr i
display (Meta x) = D.span (D.Class !:= "meta-code") <<< pure <<< text_ $ x
display (Many ds) = foldMap display ds

allsomenone :: forall a. (a -> Boolean) -> Array a -> String
allsomenone _ [] = ""
allsomenone p [a] = if p a then "Yes" else "No"
allsomenone p as =
  if Array.all p as
    then "All"
    else case Array.length (Array.filter p as) of
      0 -> "None"
      1 -> "One"
      _ -> "Some"

st = D.Style !:= "font-variant-numeric: lining-nums tabular-nums"

component :: forall lock payload. (String -> Effect Unit) -> Event String -> Domable lock payload
component setGlobal resetting =
    bussed \taCb taState' -> fold do
      let
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
          in
            if ta.end >= CU.length ta.value
            then CP.toCodePointArray ta.value
            else if r == ""
              then Array.take 1 $ CP.toCodePointArray $ CU.take 2 (CU.drop ta.start ta.value)
              else CP.toCodePointArray r
      [ D.div (st <|> D.Class !:= "sourceCode" <|> pure (xdata "lang" "Text")) $ pure $ D.pre_ $ pure $ D.code_ $ pure $ flip D.textarea [] $ oneOf
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
      , D.table (st <|> D.Class !:= "properties-table")
        [ renderInfos taSelected taValue
          [ Tuple "Code Point(s)" $ CP.length >>> Number (Just Dec)
          , Tuple "UTF-16 Code Unit(s)" $ CU.length >>> Number (Just Dec)
          ]
        , renderInfos taCP taCP $
          [ Tuple "Code Point" $ foldMap $ Unicode
          , Tuple "Decimal" $ foldMap $ fromEnum >>> Number (Just Dec)
          , Tuple "Binary" $ foldMap $ fromEnum >>> Number (Just Bin)
          , Tuple "General Category" $ foldMap $ Unicode.generalCategory >>> foldMap \cat ->
              Text (show cat) <> Text " (" <> Code (show (generalCatToUnicodeCat cat)) <> Text ")"
          ]
        , renderInfos taCPs taAllCPs
          [ uniprop { isPrint }
          , uniprop { isAlphaNum }
          , uniprop { isAlpha }
          -- , uniprop { isUpper }
          -- , uniprop { isLower }
          , uniprop { isPunctuation }
          , uniprop { isSymbol }
          , uniprop { isSpace }
          , uniprop { isControl }
          ]
        ]
      , D.button (D.OnClick <:=> (setGlobal <$> taValue) <|> D.Class !:= "add") [ text_ "Save" ]
      ]
  where
  updateTA upd = cb $
    (Event.target >=> HTMLTextArea.fromEventTarget) >>>
      traverse_ \ta -> do
        value <- HTMLTextArea.value ta
        start <- HTMLTextArea.selectionStart ta
        end <- HTMLTextArea.selectionEnd ta
        upd { value, start, end }
  renderInfos :: forall a. Event a -> Event a -> Array (Tuple String (a -> Display)) -> _
  renderInfos x y infos = D.tbody_ $ infos <#> \(Tuple name calc) -> do
    let
      uv | unsafeRefEq x y = join Tuple <<< calc <$> x
      uv = lift2 Tuple (calc <$> x) (calc <$> y)
    D.tr_ $
      [ D.td_ $ pure $ text_ name <> text_ " "
      , D.td_ $ pure $ flip switcher uv \(Tuple u v) ->
          if u == v then display u else display u <> text_ " / " <> display v
      ]




-- LowBMP < Astral < HighBMP
data Region = LowBMP | Astral | HighBMP
derive instance Eq Region
derive instance Ord Region

-- Will crash on surrogates (U+D800 to U+DFFF)
compareUTF16 :: Array CodePoint -> Array CodePoint -> Ordering
compareUTF16 l r = Array.fold
  [ Array.fold (Array.zipWith cmp16 l r)
  , compare (Array.length l) (Array.length r)
  ]
  where
  cmp16 :: CodePoint -> CodePoint -> Ordering
  cmp16 cp1 cp2 =
    (compare (regionOf cp1) (regionOf cp2)) <>
    (compare cp1 cp2)

regionOf :: CodePoint -> Region
regionOf cp = exactlyOneOf $ Array.catMaybes
  [ LowBMP <$ guard (isLowBMP cp)
  , Astral <$ guard (isAstral cp)
  , HighBMP <$ guard (isHighBMP cp)
  ]

isLowBMP :: CodePoint -> Boolean
isLowBMP = region 0x0000 0xD7FF

isHighBMP :: CodePoint -> Boolean
isHighBMP = region 0xE000 0xFFFF

isAstral :: CodePoint -> Boolean
isAstral = region 0x010000 0x10FFFF

region :: Int -> Int -> CodePoint -> Boolean
region cpLow cpHigh cp = cpLit cpLow <= cp && cp <= cpLit cpHigh

cpLit :: Int -> CodePoint
cpLit i = case toEnum i of
  Nothing -> unsafeCrashWith ""
  Just cp -> cp

exactlyOneOf :: forall a. Array a -> a
exactlyOneOf [a] = a
exactlyOneOf [] = unsafeCrashWith "No options in exactlyOneOf"
exactlyOneOf _ = unsafeCrashWith "Too many options in exactlyOneOf"
