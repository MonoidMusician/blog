module Parser.Optimized.Types where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Parser.Types (Fragment, ShiftReduce, Zipper(..))
import Parser.Types as Old
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

newtype Grammar nt tok = MkGrammar
  (Map nt (Array (Maybe (Fragment nt tok))))

fromOld :: forall nt tok. Ord nt => Old.Grammar nt Int tok -> Grammar nt tok
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

getRule :: forall nt tok. Ord nt => nt -> Maybe Int -> Grammar nt tok -> Maybe (Fragment nt tok)
getRule pName rName (MkGrammar rules) =
  unsafeFromJust "pName failed" (Map.lookup pName rules)
    # flip Array.index (fromMaybe 0 rName)
    # unsafeFromJust "rName failed"
    # unsafeFromJust "missing inside rName"
    # Just

toZipper :: forall nt tok. Int -> Fragment nt tok -> Zipper nt tok
toZipper pos = Zipper <$> Array.take pos <*> Array.drop pos

type Lookahead tok = Set tok

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

newtype State nt tok = HiddenState (CmpMap nt (CmpMap (Maybe Int) (CmpMap Int (Lookahead tok))))
derive instance newtypeState :: Newtype (State nt tok) _

type StateItem nt tok =
  { pName :: nt
  , rName :: Maybe Int
  , rule :: Zipper nt tok
  , lookahead :: Lookahead tok
  }
type StateItem' nt tok =
  { pName :: nt
  , rName :: Maybe Int
  , rule :: Int
  , lookahead :: Lookahead tok
  }

toItems :: forall nt tok m. Ord nt => Ord tok => Monoid m => Grammar nt tok -> (StateItem nt tok -> m) -> State nt tok -> m
toItems grammar f (HiddenState items) = items #
  foldMapWithIndex \pName ->
  foldMapWithIndex \rName ->
  foldMapWithIndex \pos lookahead ->
    case getRule pName rName grammar of
      Nothing -> unsafeCrashWith "missing rule"
      Just rule ->
        f { pName, rName, rule: toZipper pos rule, lookahead }

fromItem :: forall nt tok. StateItem nt tok -> State nt tok
fromItem { pName, rName, rule: Zipper before _, lookahead } =
  fromItem' { pName, rName, rule: Array.length before, lookahead }
fromItem' :: forall nt tok. StateItem' nt tok -> State nt tok
fromItem' { pName, rName, rule: pos, lookahead } =
  coerce $ Map.singleton pName $ Map.singleton rName $ Map.singleton pos lookahead

derive newtype instance eqState :: (Eq nt, Eq tok) => Eq (State nt tok)

derive newtype instance ordState :: (Ord nt, Ord tok) => Ord (State nt tok)

derive newtype instance semigroupState :: (Ord nt, Ord tok) => Semigroup (State nt tok)
derive newtype instance monoidState :: (Ord nt, Ord tok) => Monoid (State nt tok)

newtype States nt tok = States
  (Array (StateInfo nt tok))

type StateInfo nt tok =
  { items :: State nt tok
  , advance :: SemigroupMap tok (ShiftReduce Int (Fragment nt tok /\ nt /\ Maybe Int))
  , receive :: Map nt Int
  }

toOld :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> States nt tok -> Old.States Int nt (Maybe Int) tok
toOld grammar (States states) = Old.States $ states # mapWithIndex \sName x ->
  { sName
  , advance: x.advance
  , receive: x.receive
  , items: toOldState grammar x.items
  }

toOldState :: forall nt tok. Ord nt => Ord tok => Grammar nt tok -> State nt tok -> Old.State nt (Maybe Int) tok
toOldState grammar items = Old.State $ items # toItems grammar
  \{ pName, rName, rule, lookahead } ->
    [ { pName, rName, rule, lookahead: Set.toUnfoldable lookahead } ]

derive instance newtypeStates :: Newtype (States nt tok) _

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust _ (Just a) = a
unsafeFromJust msg Nothing = unsafeCrashWith ("unsafeFromJust: " <> msg)
