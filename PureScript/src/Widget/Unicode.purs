module Widget.Unicode where

import Prelude

import Control.Alternative (alt, empty, guard)
import Control.Apply (lift2)
import Control.Monad.Gen (class MonadGen, chooseBool, chooseInt, sized)
import Control.Plus ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode (generalCatToUnicodeCat, isAlpha, isAlphaNum, isControl, isMark, isPrint, isPunctuation, isSpace, isSymbol)
import Data.CodePoint.Unicode as Unicode
import Data.Codec.Argonaut as CA
import Data.Either (hush)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (fold, foldMap, intercalate, oneOf, oneOfMap, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Int as Radix
import Data.Int.Bits (shl, zshr, (.&.), (.|.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (intercalateMap)
import Data.String (CodePoint)
import Data.String as CP
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Deku.Attribute (cb, xdata, (!:=), (<:=>))
import Deku.Control (switcher, text, text_)
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
import Test.QuickCheck.Gen (Gen, randomSampleOne)
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
  Record r -> Tuple String (CodePoint -> Boolean)
uniprop r = Tuple
  (reflectSymbol (Proxy :: Proxy s) # apply fromMaybe (String.stripPrefix (String.Pattern "is")) >>> append "are " >>> (_ <> "?"))
  (Record.get (Proxy :: Proxy s) r)

data Radix
  = Dec
  | Hex
  | Bin
derive instance Eq Radix

printRadix :: Maybe Radix -> Int -> String
printRadix Nothing = show
printRadix (Just Dec) = Int.toStringAs Radix.decimal
printRadix (Just Hex) = Int.toStringAs Radix.hexadecimal >>> String.toUpper
printRadix (Just Bin) = Int.toStringAs Radix.binary

printBase :: Maybe Radix -> String
printBase Nothing = ""
printBase (Just Dec) = ""
printBase (Just Hex) = "0x"
printBase (Just Bin) = "0b"

padNumTo :: Int -> String -> String
padNumTo digits s = power "0" (digits - CU.length s) <> s

printByte :: Int -> String
printByte = padNumTo 8 <<< Int.toStringAs Radix.binary

data Display
  = Unicode CodePoint
  | Numeric String
  | Number (Maybe Radix) Int
  | Byte Int
  | Bytes Int Int
  | Text String
  | Code String
  | Meta String
  | Many (Array Display)
  | ManySep Display (Array Display)
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
display (Number mr i) = display $ Many [ Meta $ printBase mr, Numeric $ printRadix mr i ]
display (Byte b) = display $ Many [ Meta "0b", Numeric $ printByte b ]
display (Bytes n b) = display $ Many [ Meta "0b", intercalate (Meta "_") (Numeric <<< printByte <$> bytesOf n b) ]
display (Meta x) = D.span (D.Class !:= "meta-code") <<< pure <<< text_ $ x
display (Many ds) = foldMap display ds
display (ManySep sep ds) = foldMap (intercalateMap (display sep) display) (NEA.fromArray ds)

disp :: CodePoint -> String
-- disp cp | isMark cp = "◌" <> String.singleton cp
disp cp = String.singleton cp

tallyComp :: forall a. (a -> Display) -> a -> a -> Display
tallyComp f a b | f a == f b = f a
tallyComp f a b = f a <> Text " / " <> f b

allsomenone :: forall a. (a -> Boolean) -> Array a -> Array a -> Display
allsomenone _ _ [] = Text ""
allsomenone p _ as | Array.all p as = Text "All"
allsomenone p _ as | not Array.any p as = Text "None"
allsomenone p [a] [_] = if p a then Text "Yes" else Text "No"
allsomenone p [a] as = (if p a then Text "Yes" else Text "No") <> Text " / " <>
    case Array.length (Array.filter p as) of
      1 -> Text "One"
      _ -> Text "Some"
allsomenone p as bs = tallyComp tallier as bs
  where
  tallier xs = case Array.length (Array.filter p xs) of
    0 -> Text "None"
    1 -> Text "One"
    l | l == Array.length xs -> Text "All"
    _ -> Text "Some"

st = D.Style !:= "font-variant-numeric: lining-nums tabular-nums"

component :: forall lock payload. (String -> Effect Unit) -> Event String -> Domable lock payload
component setGlobal resetting =
  bussed \genNew genned -> do
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
            then maybe (only (CP.toCodePointArray ta.value)) Just $
                Array.head $ CP.toCodePointArray $ CU.take 2 (CU.drop ta.start ta.value)
            else only (CP.toCodePointArray r)
        taCPs = taState <#> \ta ->
          let r = CU.drop ta.start (CU.take ta.end ta.value)
          in
            if ta.end >= CU.length ta.value
            then CP.toCodePointArray ta.value
            else if r == ""
              then Array.take 1 $ CP.toCodePointArray $ CU.take 2 (CU.drop ta.start ta.value)
              else CP.toCodePointArray r
      [ D.button (D.OnClick <:=> (setGlobal <$> taValue) <|> D.Class !:= "add") [ text_ "Save text to URL" ]
      , D.a (D.Href !:= "") [ text_ "Shareable URL" ]
      , D.div (st <|> D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "Text")) $
          pure $ D.pre_ $ pure $ D.code_ $ pure $
            flip D.textarea [] $ oneOf
              [ D.OnInput !:= updateTA taCb
              , D.OnMousedown !:= updateTA taCb
              , D.OnMouseup !:= updateTA taCb
              , D.OnClick !:= updateTA taCb
              , D.OnKeydown !:= updateTA taCb
              , D.OnKeyup !:= updateTA taCb
              -- D.OnKeypress will lag arrow key repetitions
              , D.OnSelect !:= updateTA taCb
              , D.OnSelectionchange !:= updateTA taCb
              , D.Value <:=> resetting
              ]
      , D.div (D.Class !:= "full-width h-scroll") $ pure $ D.div (st <|> D.Class !:= "code-points unicode") $ pure $ flip switcher taAllCPs $ foldMap \cp ->
          D.span (D.Class !:= "code-point") [ text_ (disp cp) ]
      , D.div (D.Class !:= "table-wrapper") $ pure $ D.table (st <|> D.Class !:= "properties-table")
        [ renderInfos tallyComp taSelected taValue
          [ Tuple "Code Point(s)" $ CP.length >>> Number (Just Dec)
          , Tuple "UTF-16 Code Unit(s)" $ CU.length >>> Number (Just Dec)
          , Tuple "UTF-8 Bytes" $ utf8Str >>> Array.length >>> Number (Just Dec)
          , Tuple "UTF-16 Bytes" $ CU.length >>> mul 2 >>> Number (Just Dec)
          , Tuple "UTF-32 Bytes" $ CU.length >>> mul 4 >>> Number (Just Dec)
          ]
        , renderInfos tallyComp taCP taCP $
          [ Tuple "Code Point" $ foldMap $ Unicode
          , Tuple "Decimal" $ foldMap $ fromEnum >>> Number (Just Dec)
          , Tuple "Binary" $ foldMap $ fromEnum >>> Number (Just Bin)
          , Tuple "General Category" $ foldMap $ Unicode.generalCategory >>> foldMap \cat ->
              Text (show cat) <> Text " (" <> Code (show (generalCatToUnicodeCat cat)) <> Text ")"
          , Tuple "UTF-8" $ foldMap $ fromEnum >>> utf8 >>> map Byte >>> ManySep (Text " ")
          , Tuple "UTF-16" $ foldMap $ String.singleton >>> CU.toCharArray >>> map (fromEnum >>> Bytes 2) >>> ManySep (Text " ")
          , Tuple "UTF-32" $ foldMap $ fromEnum >>> Bytes 4
          ]
        , renderInfos allsomenone taCPs taAllCPs
          [ uniprop { isPrint }
          , uniprop { isAlphaNum }
          , uniprop { isAlpha }
          -- , uniprop { isUpper }
          -- , uniprop { isLower }
          , uniprop { isPunctuation }
          , uniprop { isSymbol }
          , uniprop { isSpace }
          , uniprop { isControl }
          , uniprop { isMark }
          ]
        ]
      , D.div_
        [ D.button
          ( empty
          <|> D.OnClick !:= do genNew =<< gen (Tuple <$> randomUnicode <*> randomUnicode)
          )
          [ text_ "Test"
          ]
        , text $ alt (pure "") $ map show $ genned <#> \(Tuple a b) -> Tuple
            (compare (String.fromCodePointArray a) (String.fromCodePointArray b))
            (compareUTF16 a b)
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
  renderInfos :: forall a b. (b -> a -> a -> Display) -> Event a -> Event a -> Array (Tuple String b) -> _
  renderInfos tally x y infos = D.tbody_ $ infos <#> \(Tuple name calc) -> do
    let
      uv = lift2 Tuple x y
    D.tr_ $
      [ D.td_ $ pure $ text_ name <> text_ " "
      , D.td_ $ pure $ flip switcher uv \(Tuple u v) -> display $ tally calc u v
      ]

gen :: forall a. Gen a -> Effect a
gen = randomSampleOne

genAlt :: forall m b. MonadGen m => m b -> m b -> m b
genAlt x y = chooseBool >>= if _ then x else y

infixr 4 genAlt as <%>

randomUnicode :: forall gen. MonadGen gen => gen (Array CodePoint)
randomUnicode = sized \sz ->
  sequence $ Array.replicate sz do
    fromMaybe bottom <<< toEnum <$> do
      chooseInt 0x0 0xD7FF
        <%> chooseInt 0xE000 0xFFFF
        <%> chooseInt 0x010000 0x10FFFF


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

utf8Str :: String -> Array Int
utf8Str = String.toCodePointArray >=> fromEnum >>> utf8

utf8 :: Int -> Array Int
utf8 = utf8' >>> \(NonEmpty h t) -> Array.cons h t

utf8' :: Int -> NonEmpty Array Int
utf8' i | nbitsLE i 7 = NonEmpty i []
utf8' i0 = go 6 i0 []
  where
  go allowed i acc
    | nbitsLE i allowed =
      -- Base case: set some high bits, keep a zero bit, and then the remaining
      -- high bits left from `i`
      NonEmpty (highmask (7 - allowed) .|. i) acc
    | otherwise =
      -- Pop 6 low bits from `i`, which get put in the representation with a
      -- high bit set
      go (allowed - 1) (i `zshr` 6) $
        [highmask 1 .|. (i .&. bitmask 6)] <> acc

-- Get `en` bytes of `by`
bytesOf :: Int -> Int -> Array Int
bytesOf en by =
  Array.replicate en unit # mapWithIndex \i _ ->
    (by `zshr` ((en - 1 - i) * 8)) .&. bitmask 8

bitmask :: Int -> Int
bitmask 0 = 0
bitmask 1 = 1
bitmask 2 = 3
bitmask 3 = 7
bitmask 4 = 0xF
bitmask 5 = 0x1F
bitmask 6 = 0x3F
bitmask 7 = 0x7F
bitmask 8 = 0xFF
bitmask n = (2 `shl` n) - 1

-- highmask 1 = 0b1000_0000
-- highmask 2 = 0b1100_0000
highmask :: Int -> Int
highmask n = bitmask 8 - bitmask (8 - n)

nbitsLE :: Int -> Int -> Boolean
nbitsLE i n = i .&. bitmask n == i
