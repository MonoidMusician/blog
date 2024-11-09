module Widget.Unicode where

import Prelude

import Control.Alternative (alt, guard)
import Control.Monad.Gen (class MonadGen, chooseBool, chooseInt, sized)
import Control.Plus ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.CodePoint.Unicode (generalCatToUnicodeCat, isAlpha, isAlphaNum, isControl, isMark, isPrint, isPunctuation, isSpace, isSymbol)
import Data.CodePoint.Unicode as Unicode
import Data.Codec.Argonaut as CA
import Data.Either (hush)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (foldMap, intercalate, oneOfMap)
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
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch (fetch)
import Idiolect (only, (/|\), (>==))
import Parser.Languages.Show (mkReShow)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Riverdragon.Dragon (AttrProp, Dragon)
import Riverdragon.Dragon.Bones (__textcursor, ($~~), (.$), (.$$~), (.$~~), (:.), (<!>), (<:>), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode)
import Riverdragon.River (Lake, createRiverStore)
import Riverdragon.River.Beyond (affToLake, dedup)
import Test.QuickCheck.Gen (Gen, randomSampleOne)
import Type.Proxy (Proxy(..))
import Web.Util.TextCursor as TC
import Widget (Widget, adaptInterface)

widget :: Widget
widget { interface, attrs } = do
  example <- attrs "unicode"
  let
    initial = hush $ CA.decode CA.string example
    stringInterface = adaptInterface CA.string (interface "unicode")
    resetting = oneOfMap pure initial <|> stringInterface.receive
  pure do
    component stringInterface.send resetting

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

display :: Display -> Dragon
display (Unicode cp) = display $ Meta "U+" <> Numeric do
  cp # fromEnum >>> Int.toStringAs Radix.hexadecimal >>> String.toUpper
display (Text txt) = D.text txt
display (Code c) = D.span [ D.className =:= "code" ] <<< D.text $ c
display (Numeric n) = D.span [ D.className =:= "code numeric" ] <<< D.text $ n
display (Number mr i) = display $ Many [ Meta $ printBase mr, Numeric $ printRadix mr i ]
display (Byte b) = display $ Many [ Meta "0b", Numeric $ printByte b ]
display (Bytes n b) = display $ Many [ Meta "0b", intercalate (Meta "_") (Numeric <<< printByte <$> bytesOf n b) ]
display (Meta x) = D.span [ D.className =:= "meta-code" ] <<< D.text $ x
display (Many ds) = foldMap display ds
display (ManySep sep ds) = foldMap (intercalateMap (display sep) display) (NEA.fromArray ds)

disp :: CodePoint -> String
disp cp | isMark cp = "◌" <> String.singleton cp
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

st :: Lake AttrProp
st = D.style =:= "font-variant-numeric: lining-nums tabular-nums"

component :: (String -> Effect Unit) -> Lake String -> Dragon
component setGlobal resetting = eggy \shell -> do
  { stream: genned, send: genNew } <- shell.track $ createRiverStore Nothing
  { stream: taState', send: taCb } <- shell.track $ createRiverStore Nothing
  let
    taState = dedup ((\s -> TC.mkTextCursor s "" "") <$> (pure "" <|> resetting) <|> taState')
    taValue = taState <#> TC.content
    taSelected = taState <#> \tc ->
      let r = TC.selected tc in
      if r == "" then TC.content tc else r
    taAllCPs = taValue <#> CP.toCodePointArray
    taCP = taState <#> \tc ->
      let r = TC.selected tc in
      if r == ""
        then maybe (only (CP.toCodePointArray (TC.content tc))) Just $
            Array.head $ CP.toCodePointArray $ CU.take 2 (TC.selected tc <> TC.after tc)
        else only (CP.toCodePointArray r)
    taCPs = taState <#> \tc ->
      let r = TC.selected tc in
      if TC.cursorAtEnd tc
      then CP.toCodePointArray (TC.content tc)
      else if r == ""
        then Array.take 1 $ CP.toCodePointArray $ CU.take 2 (TC.selected tc <> TC.after tc)
        else CP.toCodePointArray r
  pure $ D.Fragment
    [ D.button [ D.className =:= "add", D.onClick <!> (setGlobal <$> taValue) ] (D.text "Save text to URL")
    , D.aW "" [] (D.text "Shareable URL")
    , sourceCode "Text" :."sourceCode unicode".$ D.textarea
        [ D.on_"input" =:= updateTA taCb
        , D.on_"mousedown" =:= updateTA taCb
        , D.on_"mouseup" =:= updateTA taCb
        , D.on_"click" =:= updateTA taCb
        , D.on_"keydown" =:= updateTA taCb
        , D.on_"keyup" =:= updateTA taCb
        -- onkeypress will lag arrow key repetitions
        , D.on_"select" =:= updateTA taCb
        , D.on_"selectionchange" =:= updateTA taCb
        , D.value <:> resetting
        ]
    , D.div [ D.className =:= "full-width h-scroll" ] $ D.div [ st, D.className =:= "code-points unicode" ] $
        taAllCPs >@ foldMap \cp ->
          D.span [ D.className =:= "code-point" ] $ D.text $ disp cp
    , D.div [ D.className =:= "table-wrapper" ] $ D.table [ st, D.className =:= "data-table properties-table" ] $~~
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
        , Tuple "UTF-8" $ foldMap $ ManySep (Text " ") <<< (fromEnum >>> utf8 >== Byte)
        , Tuple "UTF-16" $ foldMap $ ManySep (Text " ") <<< (String.singleton >>> CU.toCharArray >== fromEnum >>> Bytes 2)
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
    , D.div.$~~
      [ D.buttonW "" "Test" do
          genNew =<< gen (Tuple <$> randomUnicode <*> randomUnicode)
      , D.Text $ alt (pure "") $ map show $ genned <#> \(Tuple a b) -> Tuple
          (compare (String.fromCodePointArray a) (String.fromCodePointArray b))
          (compareUTF16 a b)
      ]
    ]
  where
  updateTA upd = __textcursor upd
  renderInfos :: forall a b. (b -> a -> a -> Display) -> Lake a -> Lake a -> Array (Tuple String b) -> _
  renderInfos tally x y infos = D.tbody.$~~ infos <#> \(Tuple name calc) -> do
    D.tr.$~~
      [ D.td.$ D.text name <> D.text " "
      , D.td.$ x /|\ y >@ \(Tuple u v) -> display $ tally calc u v
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




fetchParser :: Aff String
fetchParser = _.text =<< fetch "assets/json/show-parser-states.json" {}

widgetShow :: Widget
widgetShow _ = pure $ eggy \shell -> do
  reShow <- shell.store (mkReShow <$> affToLake fetchParser)
  { send: setValue, stream: valueSet } <- shell.track $ createRiverStore Nothing
  { send: set, stream: get } <- shell.track $ createRiverStore Nothing
  formatted <- shell.store $ reShow <*> get
  lastValue <- shell.storeLast mempty formatted
  pure $ D.Fragment
    [ sourceCode "Haskell" .$ D.textarea
        [ D.onInputValue =:= set
        , D.value <:> valueSet
        , D.style =:= "height: 40vh"
        ]
    , D.div.$ D.buttonW "" "Use formatted" (setValue <<< (_ <> "\n") =<< lastValue)
    , sourceCode "Haskell" .$$~ formatted
    ]


