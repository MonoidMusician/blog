module Parser.Parserlude where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Compactable (class Compactable, compact)
import Data.Either (Either(..), either, choose, hush, blush)
import Data.Either.Nested (type (\/), (\/))
import Data.Enum (class Enum, toEnum, fromEnum)
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Foldable (class Foldable, fold, foldMap, or, and, any, all, maximum, minimum, sum, product, oneOf, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra (tt, ff, implies)
import Data.Int (hexadecimal, decimal, base36, binary)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe, optional)
import Data.Monoid (power)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong ((&&&), (***))
import Data.String as String
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.These (These(..), these)
import Data.Traversable (class Traversable, traverse, traverse_, for, for_, sequence, sequence_)
import Data.Tuple (Tuple(..), curry, uncurry, fst, snd, swap)
import Data.Tuple.Nested ((/\), type (/\))
import Dodo as D
import Dodo.Common as DC
import Effect (Effect)
import Idiolect (type (/\/), compactMap, compactMapFlipped, composeMap, composeMapFlipped, composeTraverse, composeTraverseFlipped, morph, theseing, tupling, (/\\/), (/|\), (<#?>), (<$?>), (<==<), (==<), (>==), (>==>), (\|/))
import Parser.Comb (Comb(..), named, namedRec)
import Parser.Languages (Comber, arrayOf, delim, digit, int, json, key, many, many1, many1SepBy, manyBaseSepBy, manySepBy, mopt, objectOf, opt, rawr, ws, wss, wsws, wsws', (#->), (#:))
import Parser.Languages.CSS (combinator, escape, ident, many1Comma, newline, number, percentage, string, url)
import Parser.Main.Comb as Parser.Main.Comb

main :: Effect Unit
main = Parser.Main.Comb.mainForParser parser

parser :: Comber (D.Doc Void)
parser = empty

