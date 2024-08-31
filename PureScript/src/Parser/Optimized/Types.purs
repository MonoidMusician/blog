module Parser.Optimized.Types where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\))
import Parser.Types (Fragment, ShiftReduce, Zipper(..))
import Parser.Types as Old
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

newtype Grammar space nt tok = MkGrammar
  (Map nt (Array (Maybe (Fragment space nt tok))))

fromOld :: forall space nt tok. Ord nt => Old.Grammar space nt Int tok -> Grammar space nt tok
fromOld (Old.MkGrammar allRules) = MkGrammar
  let
    pNames = Set.toMap $ Set.fromFoldable $ allRules <#> _.pName
    collectIntoArray scanned =
      Array.replicate (add 1 $ _.key $ unsafeFromJust "findMax" (Map.findMax scanned)) scanned
        # mapWithIndex Map.lookup
  in pNames # mapWithIndex \pName0 _ ->
    collectIntoArray $ Map.fromFoldable $ allRules # Array.mapMaybe
      \{ pName, rName, rule } ->
        if pName == pName0 then Just (Tuple rName rule) else Nothing

getRule :: forall space nt tok. Ord nt => nt -> Maybe Int -> Grammar space nt tok -> Maybe (Fragment space nt tok)
getRule pName rName (MkGrammar rules) =
  unsafeFromJust "pName failed" (Map.lookup pName rules)
    # flip Array.index (fromMaybe 0 rName)
    # unsafeFromJust "rName failed"
    # unsafeFromJust "missing inside rName"
    # Just

toZipper :: forall space nt tok. Int -> Fragment space nt tok -> Zipper space nt tok
toZipper pos = Zipper <$> Array.take pos <*> Array.drop pos

type Lookahead space tok = SemigroupMap tok (Additive space)

newtype CmpMap k v = CmpMap (Map k v)
derive instance newtypeCmpMap :: Newtype (CmpMap k v) _
instance semigroupCmpMap :: (Ord k, Semigroup v) => Semigroup (CmpMap k v) where
  append (CmpMap x) (CmpMap y) = coerce $ SemigroupMap x <> SemigroupMap y
instance monoidCmpMap :: (Ord k, Semigroup v) => Monoid (CmpMap k v) where
  mempty = CmpMap Map.empty

derive newtype instance Foldable (CmpMap k)
derive newtype instance FoldableWithIndex k (CmpMap k)

newtype AlwaysEq k = AlwaysEq k
instance Eq (AlwaysEq k) where
  eq _ _ = true
instance Ord (AlwaysEq k) where
  compare _ _ = EQ

instance eqCmpMap :: (Eq k, Eq v) => Eq (CmpMap k v) where
  eq (CmpMap a) (CmpMap b) =
    unsafeRefEq a b ||
      unsafeKeys a == unsafeKeys b &&
        unsafeValues a == unsafeValues b

instance ordCmpMap :: (Ord k, Ord v) => Ord (CmpMap k v) where
  compare a b | unsafeRefEq a b = EQ
  compare (CmpMap a) (CmpMap b) =
    case compare (Map.size a) (Map.size b) of
      EQ -> compare a b
      r -> r

unsafeKeys :: forall k v. Map k v -> Set k
unsafeKeys = unsafeCoerce

unsafeValues :: forall k v. Map k v -> Map (AlwaysEq k) v
unsafeValues = unsafeCoerce

newtype State space nt tok = HiddenState
  (CmpMap nt (CmpMap (Maybe Int) (CmpMap Int { lookbehind :: Additive space, lookahead :: Lookahead space tok })))
derive instance newtypeState :: Newtype (State space nt tok) _

type StateItem space nt tok =
  { pName :: nt
  , rName :: Maybe Int
  , rule :: Zipper space nt tok
  , lookbehind :: Additive space
  , lookahead :: Lookahead space tok
  }
type StateItem' space nt tok =
  { pName :: nt
  , rName :: Maybe Int
  , rule :: Int
  , lookbehind :: Additive space
  , lookahead :: Lookahead space tok
  }

toItems :: forall space nt tok m. Ord nt => Ord tok => Monoid m => Grammar space nt tok -> (StateItem space nt tok -> m) -> State space nt tok -> m
toItems grammar f (HiddenState items) = items #
  foldMapWithIndex \pName ->
  foldMapWithIndex \rName ->
  foldMapWithIndex \pos { lookbehind, lookahead } ->
    case getRule pName rName grammar of
      Nothing -> unsafeCrashWith "missing rule"
      Just rule ->
        f { pName, rName, rule: toZipper pos rule, lookbehind, lookahead }

fromItem :: forall space nt tok. StateItem space nt tok -> State space nt tok
fromItem { pName, rName, rule: Zipper before _, lookbehind, lookahead } =
  fromItem' { pName, rName, rule: Array.length before, lookbehind, lookahead }
fromItem' :: forall space nt tok. StateItem' space nt tok -> State space nt tok
fromItem' { pName, rName, rule: pos, lookbehind, lookahead } =
  coerce $ Map.singleton pName $ Map.singleton rName $ Map.singleton pos { lookbehind, lookahead }

derive newtype instance eqState :: (Eq space, Eq nt, Eq tok) => Eq (State space nt tok)

derive newtype instance ordState :: (Ord space, Ord nt, Ord tok) => Ord (State space nt tok)

derive newtype instance semigroupState :: (Semiring space, Ord space, Ord nt, Ord tok) => Semigroup (State space nt tok)
derive newtype instance monoidState :: (Semiring space, Ord space, Ord nt, Ord tok) => Monoid (State space nt tok)

newtype States space nt tok = States
  (Array (StateInfo space nt tok))

type Reduction space nt tok = Fragment space nt tok /\ nt /\ Maybe Int

type StateInfo space nt tok =
  { items :: State space nt tok
  , advance :: SemigroupMap tok (Additive space /\ ShiftReduce Int (Reduction space nt tok))
  , receive :: Map nt Int
  }

toOld :: forall space nt tok. Ord nt => Ord tok => Grammar space nt tok -> States space nt tok -> Old.States Int space nt (Maybe Int) tok
toOld grammar (States states) = Old.States $ states # mapWithIndex \sName x ->
  { sName
  , advance: x.advance
  , receive: x.receive
  , items: toOldState grammar x.items
  }

toOldState :: forall space nt tok. Ord nt => Ord tok => Grammar space nt tok -> State space nt tok -> Old.State space nt (Maybe Int) tok
toOldState grammar items = Old.State $ items # toItems grammar
  \{ pName, rName, rule, lookbehind, lookahead: SemigroupMap lookahead } ->
    [ { pName, rName, rule, lookbehind, lookahead: (\(Tuple x (Additive y)) -> Tuple y x) <$> Map.toUnfoldable lookahead } ]

derive instance newtypeStates :: Newtype (States space nt tok) _

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust _ (Just a) = a
unsafeFromJust msg Nothing = unsafeCrashWith ("unsafeFromJust: " <> msg)
