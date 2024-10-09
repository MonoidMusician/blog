module Parser.Parserlude ( module ReExports ) where

import Parser.Comb.Comber
import Parser.Comb.Comber as ReExports
import Prelude
import Prelude as ReExports

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Int as Int
import Data.Set as Set
import Data.String as String
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.String.Regex (regex, source, flags, test, match, replace, replace', search, split) as Re
import Data.String.Regex.Flags (dotAll, global, ignoreCase, multiline, noFlags, sticky, unicode) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Dodo as D
import Dodo.Common as DC

import Control.Alt (class Alt, (<|>))
import Control.Alt (class Alt, (<|>)) as ReExports
import Control.Alternative (class Alternative, empty)
import Control.Alternative (class Alternative, empty) as ReExports
import Control.Apply (lift2)
import Control.Apply (lift2) as ReExports
import Control.Plus (class Plus, empty)
import Control.Plus (class Plus, empty) as ReExports
import Data.Argonaut (Json)
import Data.Argonaut (Json) as ReExports
import Data.Argonaut as Json
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (NonEmptyArray) as ReExports
import Data.Array.NonEmpty as NEA
import Data.Compactable (class Compactable, compact)
import Data.Compactable (class Compactable, compact) as ReExports
import Data.Either (Either(..), either, choose, hush, blush)
import Data.Either (Either(..), either, choose, hush, blush) as ReExports
import Data.Either.Nested (type (\/), (\/))
import Data.Either.Nested (type (\/), (\/)) as ReExports
import Data.Enum (class Enum, toEnum, fromEnum)
import Data.Enum (class Enum, toEnum, fromEnum) as ReExports
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Filterable (class Filterable, filter, filterMap) as ReExports
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
import Data.Map (Map, SemigroupMap(..))
import Data.Map (Map, SemigroupMap(..)) as ReExports
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe, optional)
import Data.Maybe (Maybe(..), maybe, fromMaybe, optional) as ReExports
import Data.Monoid (power)
import Data.Monoid (power) as ReExports
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Additive (Additive(..)) as ReExports
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Endo (Endo(..)) as ReExports
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Monoid.Multiplicative (Multiplicative(..)) as ReExports
import Data.Newtype (unwrap, wrap, un, class Newtype)
import Data.Newtype (unwrap, wrap, un, class Newtype) as ReExports
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Choice ((|||)) as ReExports
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
import Data.These (These(..), these)
import Data.These (These(..), these) as ReExports
import Data.Traversable (class Traversable, traverse, traverse_, for, for_, sequence, sequence_)
import Data.Traversable (class Traversable, traverse, traverse_, for, for_, sequence, sequence_) as ReExports
import Data.Tuple (Tuple(..), curry, uncurry, fst, snd, swap)
import Data.Tuple (Tuple(..), curry, uncurry, fst, snd, swap) as ReExports
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple.Nested ((/\), type (/\)) as ReExports
import Dodo as D
import Dodo.Common as DC
import Effect (Effect)
import Effect (Effect) as ReExports
import Effect.Aff (Aff)
import Effect.Aff (Aff) as ReExports
import Effect.Class.Console (log, logShow)
import Effect.Class.Console (log, logShow) as ReExports
import Idiolect (type (/\/), compactMap, compactMapFlipped, composeMap, composeMapFlipped, composeTraverse, composeTraverseFlipped, intercalateMap, morph, theseing, tupling, (/\\/), (/|\), (<#?>), (<$?>), (<==<), (==<), (>==), (>==>), (\|/))
import Idiolect (type (/\/), compactMap, compactMapFlipped, composeMap, composeMapFlipped, composeTraverse, composeTraverseFlipped, intercalateMap, morph, theseing, tupling, (/\\/), (/|\), (<#?>), (<$?>), (<==<), (==<), (>==), (>==>), (\|/)) as ReExports
import Parser.Comb (Comb(..))
import Parser.Comb (Comb(..)) as ReExports
import Parser.Languages (digit, int, json, string, digit, int, number)
import Parser.Languages (digit, int, json, string, digit, int, number) as ReExports
import Parser.Languages.CSS (combinator, escape, ident, many1Comma, newline, percentage, url)
import Parser.Languages.CSS (combinator, escape, ident, many1Comma, newline, percentage, url) as ReExports
import Parser.Main.Comb as Parser.Main.Comb
import Prim.TypeError (class Fail, Beside, Quote, Text)
import Safe.Coerce (coerce)
import Safe.Coerce (coerce) as ReExports
import Type.Equality (class TypeEquals, proof)
import Type.Equality (class TypeEquals, proof) as ReExports

main :: Effect Unit
main = Parser.Main.Comb.mainForParser parser

parser :: Comber (D.Doc Void)
parser = empty

class StringOrDoc t where
  toDoc :: t -> D.Doc Void

instance StringOrDoc String where
  toDoc = D.text
else instance TypeEquals ann Void => StringOrDoc (D.Doc ann) where
  toDoc = proof
else instance Fail (Beside (Text "Parser must produce String or Doc, not ") (Quote t)) => StringOrDoc t where
  toDoc = mempty
