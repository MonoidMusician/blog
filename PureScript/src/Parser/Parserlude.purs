module Parser.Parserlude ( module ReExports ) where

import Parser.Comb.Comber
import Parser.Comb.Comber as ReExports
import Prelude
import Prelude as ReExports

import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.List (List)
import Data.List (List) as ReExports
import Data.List as List
import Data.Map as Map
import Data.Int as Int
import Data.Set as Set
import Data.String as String
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.String.Regex (regex, source, flags, test, match, replace, replace', search, split) as Re
import Data.String.Regex.Flags (dotAll, global, ignoreCase, multiline, noFlags, sticky, unicode) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Dodo as T
import Dodo.Common as T

import Control.Alt (class Alt, (<|>))
import Control.Alt (class Alt, (<|>)) as ReExports
import Control.Alternative (class Alternative, empty)
import Control.Alternative (class Alternative, empty) as ReExports
import Control.Apply (lift2)
import Control.Apply (lift2) as ReExports
import Control.Comonad (extract)
import Control.Comonad (extract) as ReExports
import Control.Plus (class Plus, empty)
import Control.Plus (class Plus, empty) as ReExports
import Data.Argonaut (Json)
import Data.Argonaut (Json) as ReExports
import Data.Argonaut as Json
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (NonEmptyArray) as ReExports
import Data.Array.NonEmpty as NEA
import Data.Compactable (class Compactable, compact, separate)
import Data.Compactable (class Compactable, compact, separate) as ReExports
import Data.Either (Either(..), either, choose, note, hush, blush)
import Data.Either (Either(..), either, choose, note, hush, blush) as ReExports
import Data.Either.Nested (type (\/), (\/))
import Data.Either.Nested (type (\/), (\/)) as ReExports
import Data.Enum (class Enum, toEnum, fromEnum)
import Data.Enum (class Enum, toEnum, fromEnum) as ReExports
import Data.Filterable (class Filterable, filter, filterMap, eitherBool, maybeBool)
import Data.Filterable (class Filterable, filter, filterMap, eitherBool, maybeBool) as ReExports
import Data.Foldable (class Foldable, fold, foldMap, or, and, any, all, maximum, minimum, sum, product, oneOf, oneOfMap, intercalate)
import Data.Foldable (class Foldable, fold, foldMap, or, and, any, all, maximum, minimum, sum, product, oneOf, oneOfMap, intercalate) as ReExports
import Data.Semigroup.Foldable (class Foldable1, fold1, foldMap1)
import Data.Semigroup.Foldable (class Foldable1, fold1, foldMap1) as ReExports
import Data.FunctorWithIndex (mapWithIndex)
import Data.FunctorWithIndex (mapWithIndex) as ReExports
import Data.HeytingAlgebra (tt, ff, implies)
import Data.HeytingAlgebra (tt, ff, implies) as ReExports
import Data.Identity (Identity(..))
import Data.Identity (Identity(..)) as ReExports
import Data.Int (hexadecimal, decimal, base36, binary)
import Data.Int (hexadecimal, decimal, base36, binary) as ReExports
import Data.Int as Int
import Data.Int.Bits ((.&.), (.^.), (.|.))
import Data.Int.Bits ((.&.), (.^.), (.|.)) as ReExports
import Data.Int.Bits as Int.Bits
import Data.Map (Map, SemigroupMap(..))
import Data.Map (Map, SemigroupMap(..)) as ReExports
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, optional, isNothing, isJust)
import Data.Maybe (Maybe(..), maybe, fromMaybe, optional, isNothing, isJust) as ReExports
import Data.Monoid (power)
import Data.Monoid (power) as ReExports
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Additive (Additive(..)) as ReExports
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Conj (Conj(..)) as ReExports
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Disj (Disj(..)) as ReExports
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Endo (Endo(..)) as ReExports
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Monoid.Multiplicative (Multiplicative(..)) as ReExports
import Data.Newtype (unwrap, wrap, un, class Newtype)
import Data.Newtype (unwrap, wrap, un, class Newtype) as ReExports
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor (dimap, lcmap) as ReExports
import Data.Profunctor.Choice ((|||), (+++))
import Data.Profunctor.Choice ((|||), (+++)) as ReExports
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Star (Star(..)) as ReExports
import Data.Profunctor.Strong ((&&&), (***))
import Data.Profunctor.Strong ((&&&), (***)) as ReExports
import Data.Set (Set)
import Data.Set (Set) as ReExports
import Data.Set as Set
import Data.String as String
import Data.String (Pattern(..), Replacement(..))
import Data.String (Pattern(..), Replacement(..)) as ReExports
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.String.Regex (regex, source, flags, test, match, replace, replace', search, split) as Re
import Data.String.Regex.Flags (dotAll, global, ignoreCase, multiline, noFlags, sticky, unicode) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol) as ReExports
import Data.These (These(..), these)
import Data.These (These(..), these) as ReExports
import Data.Traversable (class Traversable, traverse, traverse_, for, for_, sequence, sequence_, scanl, scanr, mapAccumL, mapAccumR)
import Data.Traversable (class Traversable, traverse, traverse_, for, for_, sequence, sequence_, scanl, scanr, mapAccumL, mapAccumR) as ReExports
import Data.Tuple (Tuple(..), curry, uncurry, fst, snd, swap)
import Data.Tuple (Tuple(..), curry, uncurry, fst, snd, swap) as ReExports
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple.Nested ((/\), type (/\)) as ReExports
import Effect (Effect)
import Effect (Effect) as ReExports
import Effect.Aff (Aff)
import Effect.Aff (Aff) as ReExports
import Effect.Class.Console (log, logShow)
import Effect.Class.Console (log, logShow) as ReExports
import Effect.Class.Console as Console
import Idiolect (type (/\/), CommonPrefix(..), EffectArrow, composeMap, composeMapFlipped, composeTraverse, composeTraverseFlipped, filterFst, filterKey, filterMapFlipped, filterSnd, foldMapFlipped, foldMapWithIndexFlipped, intercalateMap, mapWithIndexFlipped, morph, multiplexing, nonEmpty, only, partitionMapFlipped, theseing, tripleQuoted, tupling, (#..), (#:..), (..$), (..:$), (/#?\), (/$?\), (/\\/), (/|\), (:<$>), (<#..>), (<#:..>), (<#>:), (<#?>), (<$?>), (<..$>), (<..:$>), (<==<), (==<), (>==), (>==>), (\|/))
import Idiolect (type (/\/), CommonPrefix(..), EffectArrow, composeMap, composeMapFlipped, composeTraverse, composeTraverseFlipped, filterFst, filterKey, filterMapFlipped, filterSnd, foldMapFlipped, foldMapWithIndexFlipped, intercalateMap, mapWithIndexFlipped, morph, multiplexing, nonEmpty, only, partitionMapFlipped, theseing, tripleQuoted, tupling, (#..), (#:..), (..$), (..:$), (/#?\), (/$?\), (/\\/), (/|\), (:<$>), (<#..>), (<#:..>), (<#>:), (<#?>), (<$?>), (<..$>), (<..:$>), (<==<), (==<), (>==), (>==>), (\|/)) as ReExports
import Parser.Comb (Comb(..))
import Parser.Comb (Comb(..)) as ReExports
import Parser.Languages (digit, int, json, string, digit, int, number)
import Parser.Languages (digit, int, json, string, digit, int, number) as ReExports
import Parser.Languages.CSS (combinator, escape, ident, many1Comma, newline, percentage, url)
import Parser.Languages.CSS (combinator, escape, ident, many1Comma, newline, percentage, url) as ReExports
import Riverdragon.Dragon.Bones ((.<>), (<>.))
import Parser.Main.Live as Parser.Main.Live
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError (class Fail, Beside, Quote, Text)
import Record as Record
import Safe.Coerce (coerce)
import Safe.Coerce (coerce) as ReExports
import Type.Equality (class TypeEquals, proof)
import Type.Equality (class TypeEquals, proof) as ReExports
import Type.Proxy (Proxy(..))
import Type.Proxy (Proxy(..)) as ReExports

main :: Effect Unit
main = Parser.Main.Live.mainForParser parser

parser :: Comber (T.Doc Void)
parser = empty

class StringOrDoc t where
  toDoc :: t -> T.Doc Void

instance StringOrDoc String where
  toDoc = T.text
else instance TypeEquals ann Void => StringOrDoc (T.Doc ann) where
  toDoc = proof
else instance Fail (Beside (Text "Parser must produce String or Doc, not ") (Quote t)) => StringOrDoc t where
  toDoc = mempty
