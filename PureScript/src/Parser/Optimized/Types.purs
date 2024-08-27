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

newtype Grammar meta nt tok = MkGrammar
  (Map nt (Array (Maybe (Fragment meta nt tok))))

fromOld :: forall meta nt tok. Ord nt => Old.Grammar meta nt Int tok -> Grammar meta nt tok
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

getRule :: forall meta nt tok. Ord nt => nt -> Maybe Int -> Grammar meta nt tok -> Maybe (Fragment meta nt tok)
getRule pName rName (MkGrammar rules) =
  unsafeFromJust "pName failed" (Map.lookup pName rules)
    # flip Array.index (fromMaybe 0 rName)
    # unsafeFromJust "rName failed"
    # unsafeFromJust "missing inside rName"
    # Just

toZipper :: forall meta nt tok. Int -> Fragment meta nt tok -> Zipper meta nt tok
toZipper pos = Zipper <$> Array.take pos <*> Array.drop pos

type Lookahead meta tok = SemigroupMap tok (Additive meta)

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

newtype State meta nt tok = HiddenState
  (CmpMap nt (CmpMap (Maybe Int) (CmpMap Int { lookbehind :: Additive meta, lookahead :: Lookahead meta tok })))
derive instance newtypeState :: Newtype (State meta nt tok) _

type StateItem meta nt tok =
  { pName :: nt
  , rName :: Maybe Int
  , rule :: Zipper meta nt tok
  , lookbehind :: Additive meta
  , lookahead :: Lookahead meta tok
  }
type StateItem' meta nt tok =
  { pName :: nt
  , rName :: Maybe Int
  , rule :: Int
  , lookbehind :: Additive meta
  , lookahead :: Lookahead meta tok
  }

toItems :: forall meta nt tok m. Ord nt => Ord tok => Monoid m => Grammar meta nt tok -> (StateItem meta nt tok -> m) -> State meta nt tok -> m
toItems grammar f (HiddenState items) = items #
  foldMapWithIndex \pName ->
  foldMapWithIndex \rName ->
  foldMapWithIndex \pos { lookbehind, lookahead } ->
    case getRule pName rName grammar of
      Nothing -> unsafeCrashWith "missing rule"
      Just rule ->
        f { pName, rName, rule: toZipper pos rule, lookbehind, lookahead }

fromItem :: forall meta nt tok. StateItem meta nt tok -> State meta nt tok
fromItem { pName, rName, rule: Zipper before _, lookbehind, lookahead } =
  fromItem' { pName, rName, rule: Array.length before, lookbehind, lookahead }
fromItem' :: forall meta nt tok. StateItem' meta nt tok -> State meta nt tok
fromItem' { pName, rName, rule: pos, lookbehind, lookahead } =
  coerce $ Map.singleton pName $ Map.singleton rName $ Map.singleton pos { lookbehind, lookahead }

derive newtype instance eqState :: (Eq meta, Eq nt, Eq tok) => Eq (State meta nt tok)

derive newtype instance ordState :: (Ord meta, Ord nt, Ord tok) => Ord (State meta nt tok)

derive newtype instance semigroupState :: (Semiring meta, Ord meta, Ord nt, Ord tok) => Semigroup (State meta nt tok)
derive newtype instance monoidState :: (Semiring meta, Ord meta, Ord nt, Ord tok) => Monoid (State meta nt tok)

newtype States meta nt tok = States
  (Array (StateInfo meta nt tok))

type Reduction meta nt tok = Fragment meta nt tok /\ nt /\ Maybe Int

type StateInfo meta nt tok =
  { items :: State meta nt tok
  , advance :: SemigroupMap tok (Additive meta /\ ShiftReduce Int (Reduction meta nt tok))
  , receive :: Map nt Int
  }

toOld :: forall meta nt tok. Ord nt => Ord tok => Grammar meta nt tok -> States meta nt tok -> Old.States Int meta nt (Maybe Int) tok
toOld grammar (States states) = Old.States $ states # mapWithIndex \sName x ->
  { sName
  , advance: x.advance
  , receive: x.receive
  , items: toOldState grammar x.items
  }

toOldState :: forall meta nt tok. Ord nt => Ord tok => Grammar meta nt tok -> State meta nt tok -> Old.State meta nt (Maybe Int) tok
toOldState grammar items = Old.State $ items # toItems grammar
  \{ pName, rName, rule, lookbehind, lookahead: SemigroupMap lookahead } ->
    [ { pName, rName, rule, lookbehind, lookahead: (\(Tuple x (Additive y)) -> Tuple y x) <$> Map.toUnfoldable lookahead } ]

derive instance newtypeStates :: Newtype (States meta nt tok) _

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust _ (Just a) = a
unsafeFromJust msg Nothing = unsafeCrashWith ("unsafeFromJust: " <> msg)
