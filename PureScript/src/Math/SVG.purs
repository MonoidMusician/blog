module Math.SVG where

import Prelude

import Control.Monad.State (State, evalState, get, put, runState, state)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Data.Newtype (un)
import Data.Number (nan)
import Data.String as String
import Data.Traversable (foldMap, oneOf, sequence, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Idiolect (chooseBy, (#..), (<>$))
import Math.Bezier (castUp, evalBSS)
import Math.Matrix (Afn2(..), Afn3(..), B32, BBox2, Bez1(..), Bez2(..), Bez3(..), Fraction, LTF(..), V2, Vec2(..), bounds2bez, extents, tf, tfI, (<.), (<>-))
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many, many1) as P
import Parsing.String (string) as P
import Parsing.String.Basic (number, whiteSpace) as P
import Riverdragon.Dragon.Bones (inkscapeNS)
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.NamespaceURI (NamespaceURI)
import Web.DOM.ParentNode (QuerySelector(..))

newtype XPath = XPath String

type CurryFn16 i o =
  i -> i -> i -> i ->
  i -> i -> i -> i ->
  i -> i -> i -> i ->
  i -> i -> i -> i ->
  o
afn2 :: forall s. CurryFn16 s (Afn2 s)
afn2 c1 c2 _ _  c3 c4 _ _  _ _ _ _  t1 t2 _ _ = Afn2 c1 c2 c3 c4 t1 t2
afn3 :: forall s. CurryFn16 s (Afn3 s)
afn3 c1 c2 c3 _  c4 c5 c6 _  c7 c8 c9 _  t1 t2 t3 _ = Afn3 c1 c2 c3 c4 c5 c6 c7 c8 c9 t1 t2 t3

newtype XMLNode = XMLNode
  { tagName :: String
  , namespaceURI :: NamespaceURI
  , parent :: Unit -> Array XMLNode
  , childNodes :: forall r. { text :: String -> r, comment :: String -> r, node :: XMLNode -> r } -> Array r
  , querySelector :: QuerySelector -> Array XMLNode
  , querySelectorAll :: QuerySelector -> Array XMLNode
  , xpath :: XPath -> Array XMLNode
  , getAttributes :: Unit -> Array { name :: AttrName, localName :: String, namespaceURI :: NamespaceURI, value :: String }
  , getAttribute :: Array NamespaceURI -> AttrName -> Array String
  , getPathData :: Boolean -> Array { type :: String, values :: Array Number }
  , serializeToXML :: Unit -> String

  -- SVG
  -- 4x4 homogeneous transformation matrices
  , getTransformListAs :: forall mat.
      CurryFn16 Number mat ->
      Array mat
  , getBBox ::
      { fill :: Boolean, stroke :: Boolean, markers :: Boolean, clipped :: Boolean } ->
      { x :: Number, y :: Number, width :: Number, height :: Number }
  , getCTMAs :: forall mat. CurryFn16 Number mat -> mat
  }
type SVGNode = XMLNode

foreign import _parseMIME :: String -> (String -> Array XMLNode)

parseHTML :: String -> Maybe XMLNode
parseHTML = Array.head <<< _parseMIME "text/html"

parseXML :: String -> Maybe XMLNode
parseXML = Array.head <<< _parseMIME "application/xml"

parseSVG :: String -> Maybe SVGNode
parseSVG = Array.head <<< _parseMIME "image/svg+xml"

serializeToXML :: XMLNode -> String
serializeToXML (XMLNode n) = n.serializeToXML unit


data ChildNode = Text String | Comment String | ChildNode XMLNode
childNodes :: XMLNode -> Array ChildNode
childNodes (XMLNode n) = n.childNodes { text: Text, comment: Comment, node: ChildNode }

parent :: XMLNode -> Maybe XMLNode
parent (XMLNode n) = Array.head $ n.parent unit

querySelector :: QuerySelector -> XMLNode -> Maybe XMLNode
querySelector query (XMLNode n) = Array.head $ n.querySelector query

querySelectorAll :: QuerySelector -> XMLNode -> Array XMLNode
querySelectorAll query (XMLNode n) = n.querySelectorAll query

xpath :: XPath -> XMLNode -> Array XMLNode
xpath query (XMLNode n) = n.xpath query

getChildren :: XMLNode -> Array XMLNode
getChildren = querySelectorAll (QuerySelector ":scope > *")

firstElementChild :: XMLNode -> Array XMLNode
firstElementChild = querySelectorAll (QuerySelector ":scope > :first-child")

lastElementChild :: XMLNode -> Array XMLNode
lastElementChild = querySelectorAll (QuerySelector ":scope > :last-child")


getAttributes :: XMLNode -> Array { name :: AttrName, localName :: String, namespaceURI :: NamespaceURI, value :: String }
getAttributes (XMLNode n) = n.getAttributes unit

getAttribute :: XMLNode -> Maybe NamespaceURI -> AttrName -> Maybe String
getAttribute (XMLNode n) ns name = Array.head $ n.getAttribute (Array.fromFoldable ns) name

getPathData :: XMLNode -> Boolean -> Array { type :: String, values :: Array Number }
getPathData (XMLNode n) normalized = n.getPathData normalized

getId :: XMLNode -> Maybe String
getId n = getAttribute n Nothing (AttrName "id")

getInkscapeLabel :: SVGNode -> Maybe String
getInkscapeLabel n = getAttribute n (Just inkscapeNS) (AttrName "label")

findInkscapeLabelPath :: Array String -> SVGNode -> Array SVGNode
findInkscapeLabelPath labels = xpath $ XPath $ "." <>$
  labels #.. \label -> "//*[@inkscape:label="<>show label<>"]"

findInkscapeLabel :: String -> SVGNode -> Array SVGNode
findInkscapeLabel = Array.singleton >>> findInkscapeLabelPath


getCTMAs :: forall mat. CurryFn16 Number mat -> SVGNode -> mat
getCTMAs mkMat (XMLNode n) = n.getCTMAs mkMat

getCTM2 :: SVGNode -> Afn2 Number
getCTM2 = getCTMAs afn2
getCTM3 :: SVGNode -> Afn3 Number
getCTM3 = getCTMAs afn3

getTransformListAs :: forall mat. CurryFn16 Number mat -> SVGNode -> Array mat
getTransformListAs mkMat (XMLNode n) = n.getTransformListAs mkMat

getTransformList2 :: SVGNode -> Array (Afn2 Number)
getTransformList2 = getTransformListAs afn2
getTransformList3 :: SVGNode -> Array (Afn3 Number)
getTransformList3 = getTransformListAs afn3

getTransform2 :: SVGNode -> Afn2 Number
getTransform2 = getTransformList2 >>> Array.foldl (<.) tfI
getTransform3 :: SVGNode -> Afn3 Number
getTransform3 = getTransformList3 >>> Array.foldl (<.) tfI


-- Maintain initial point, previous point, and previous quadratic and cubic handles
type ParseState = { init :: V2, p0 :: V2, p1 :: Maybe V2, p2 :: Maybe V2 }
pathDataParser :: Parser String (State ParseState (Array B32))
pathDataParser =
  map (join >>> sequence >>> map join) $ ws *> do
    P.many $ oneOf
      [ absOrRel "M" ado
          p1 <- point
          in [] <$ put (s1 p1)
      , absOrRel "C" ado
          p1 <- point
          p2 <- point
          p3 <- point
          in mk \{ p0 } -> cubic p0 p1 p2 p3
      , absOrRel "S" ado
          p2 <- point
          p3 <- point
          in do
            p1 <- get <#> \{ p0, p2: prev } ->
              p0 <> (p0 <>- fromMaybe p0 prev)
            mk \{ p0 } -> cubic p0 p1 p2 p3
      , absOrRel "Q" ado
          p1 <- point
          p2 <- point
          in mk \{ p0 } -> quadr p0 p1 p2
      , absOrRel "T" ado
          p2 <- point
          in do
            p1 <- get <#> \{ p0, p1: prev } ->
              p0 <> (p0 <>- fromMaybe p0 prev)
            mk \{ p0 } -> quadr p0 p1 p2
      , absOrRel "L" ado
          p1 <- point
          in mk \{ p0 } -> line p0 p1
      , P.string "Z" *> ws *> pure [mk \{ p0, init } -> line p0 init]

      , cmd "h" ado
          dx <- num
          in mk \{ p0: V2 x0 y0 } -> line (V2 x0 y0) (V2 (x0 + dx) y0)
      , cmd "v" ado
          dy <- num
          in mk \{ p0: V2 x0 y0 } -> line (V2 x0 y0) (V2 x0 (y0 + dy))
      , cmd "H" ado
          x1 <- num
          in mk \{ p0: V2 x0 y0 } -> line (V2 x0 y0) (V2 x1 y0)
      , cmd "V" ado
          y1 <- num
          in mk \{ p0: V2 x0 y0 } -> line (V2 x0 y0) (V2 x0 y1)
      ] <* ws
  where
  s1 p0 = { p0, init: p0, p1: Nothing, p2: Nothing }
  mk f = state \s -> f s s.init
  cubic p0 p1 p2 p3 init = [B3 p0 p1 p2 p3]      /\ { p0: p3, init, p1: Nothing, p2: Just p2 }
  quadr p0 p1 p2 init = [castUp $ B2 p0 p1 p2]   /\ { p0: p2, init, p1: Just p1, p2: Nothing }
  line p0 p1 init = [castUp $ castUp $ B1 p0 p1] /\ { p0: p1, init, p1: Nothing, p2: Nothing }

  ws = P.whiteSpace *> optional (P.string "," *> P.whiteSpace)
  num = P.number <* ws
  point :: Compose ((->) Boolean) (Compose (Parser String) (State ParseState)) V2
  point = Compose \rel -> Compose ado
    x <- num
    y <- num
    in case rel of
      false -> pure $ V2 x y
      true -> get <#> \{ p0 } -> p0 <> V2 x y
  cmd :: String -> Parser String (State ParseState (Array B32)) -> Parser String (Array (State ParseState (Array B32)))
  cmd name parts = NEA.toArray <$> do
    P.string name *> ws *> P.many1 parts
  absOrRel :: String -> Compose ((->) Boolean) (Compose (Parser String) (State ParseState)) (State ParseState (Array B32)) -> Parser String (Array (State ParseState (Array B32)))
  absOrRel name parts = oneOf
    [ cmd (String.toUpper name) (map join $ un Compose $ un Compose parts false)
    , cmd (String.toLower name) (map join $ un Compose $ un Compose parts true)
    ]

parsePathData :: String -> Array B32
parsePathData s = runParser s pathDataParser # foldMap
  (flip evalState mempty)

getLocalBez :: SVGNode -> Array B32
getLocalBez n = parsePathData $
  getPathData n true #.. \r ->
    r.type <> " " <> Array.intercalate " " (show <$> r.values) <> " "

getCTMBez :: SVGNode -> Array B32
getCTMBez = do
  ctm <- getCTM2
  curves <- getLocalBez
  pure $ tf (LTF ctm) <$> curves

pathDataPrinter :: B32 -> State V2 String
pathDataPrinter (B3 p0 p1 p2 p3) = do
  pp <- get <* put p3
  pure $ (if p0 == pp then "" else "M" <> point p0 <> " ") <>
    "C" <> point p1 <> point p2 <> point p3
  where
  point (V2 x y) = " " <> show x <> "," <> show y

printPathData :: Array B32 -> String
printPathData = traverse pathDataPrinter
  >>> flip evalState (pure nan)
  >>> String.joinWith " "

-- https://svgwg.org/svg2-draft/coords.html#ComputingAViewportsTransform
calcViewBoxTransform ::
  { viewBox :: BBox2 Number
  , viewport :: BBox2 Number
  , alignAt :: Maybe (Vec2 Fraction)
  , slice :: Boolean {- false: meet, true: slice -}
  } -> Afn2 Number
calcViewBoxTransform { viewBox, viewport, alignAt, slice } =
  let
    chooseScale = chooseBy $ alignAt $> if slice
      then max {- slice -}
      else min {- meet -}
    scales@(V2 scaleX scaleY) = chooseScale $
      (/) <$> extents viewport <*> extents viewBox
    V2 tX tY = ado
      vpb <- viewport <#> bounds2bez
      vbb <- viewBox <#> bounds2bez
      scale <- scales
      at <- alignAt # fromMaybe mempty
      in evalBSS vpb at - scale * evalBSS vbb at
  in Afn2  scaleX zero  zero scaleY  tX tY
