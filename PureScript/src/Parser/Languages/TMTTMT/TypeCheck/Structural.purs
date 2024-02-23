module Parser.Languages.TMTTMT.TypeCheck.Structural where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (WriterT, runWriter, tell)
import Data.Array (fold)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Identity (Identity)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), all, any, foldMap, foldr, (:))
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_, sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Idiolect (intercalateMap)
import Parser.Languages.TMTTMT.Types (Pattern(..))
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Uncurried.RWSE (RWSE, evalRWSE)

type TmVar = String
type TyVar = String

type Items = Array
type Options = Array

data Value = ScalarValue String | VectorValue (Items Value)

data BasicSubsetF a
  = Singleton String
  | AnyScalar
  | ListOf a
  | Tupled (Items a)
derive instance eqBasicSubsetF :: Eq a => Eq (BasicSubsetF a)

newtype BasicSubset = BasicSubset (BasicSubsetF BasicSubset)

singleton :: Value -> BasicSubset
singleton = BasicSubset <<< case _ of
  ScalarValue s -> Singleton s
  VectorValue vs -> Tupled (singleton <$> vs)

demonstrate :: BasicSubset -> Value
demonstrate (BasicSubset bs) = case bs of
  Singleton s -> ScalarValue s
  AnyScalar -> ScalarValue "0"
  ListOf bs' -> VectorValue [demonstrate bs']
  Tupled vs -> VectorValue (demonstrate <$> vs)

overlapF :: forall a. (a -> a -> Maybe a) -> BasicSubsetF a -> BasicSubsetF a -> Maybe (BasicSubsetF a)
overlapF f = case _, _ of
    Singleton v1, Singleton v2 ->
      if v1 == v2 then Just (Singleton v1) else Nothing
    Singleton "", AnyScalar -> Nothing
    AnyScalar, Singleton "" -> Nothing
    AnyScalar, Singleton s -> Just (Singleton s)
    Singleton s, AnyScalar -> Just (Singleton s)
    AnyScalar, AnyScalar -> Just AnyScalar

    Singleton _, ListOf _ -> Nothing
    ListOf _, Singleton _ -> Nothing
    Singleton _, Tupled _ -> Nothing
    Tupled _, Singleton _ -> Nothing
    AnyScalar, ListOf _ -> Nothing
    ListOf _, AnyScalar -> Nothing
    AnyScalar, Tupled _ -> Nothing
    Tupled _, AnyScalar -> Nothing

    ListOf _, Tupled [] -> Nothing
    Tupled [], ListOf _ -> Nothing
    ListOf p_, Tupled ps ->
      Tupled <$> traverse (f p_) ps
    Tupled ps, ListOf p_ ->
      Tupled <$> traverse (flip f p_) ps

    ListOf p1_, ListOf p2_ ->
      ListOf <$> f p1_ p2_
    Tupled ps1, Tupled ps2 ->
      if Array.length ps1 == Array.length ps2
      then Tupled <$> sequence (Array.zipWith f ps1 ps2)
      else Nothing

overlapBasic :: BasicSubset -> BasicSubset -> Maybe BasicSubset
overlapBasic (BasicSubset a) (BasicSubset b) = BasicSubset <$> overlapF overlapBasic a b

data Functional
  = Function Functional Functional
  | Concrete (BasicSubsetF Functional)
  | TyVar TyVar
  | Union (Options Functional)

derive instance eqFunctional :: Eq Functional

instance monoidFunctional :: Monoid Functional where
  mempty = Union []
instance semigroupFunctional :: Semigroup Functional where
  append (Union []) vdom = vdom
  append vdom (Union []) = vdom
  append (Union v1s) (Union v2s) = Union (v1s <> v2s)

  -- Keep flattening, to keep it associative
  append v1 (Union v2s) = Union ([v1] <> v2s)
  append (Union v1s) v2 = Union (v1s <> [v2])

  -- Finally, if none of the above cases apply,
  -- we wrap it up in a two-element array:
  append v1 v2 = Union [v1, v2]

type SubsetsWithFunctions = Options SubsetWithFunctions
type SubsetWithFunctions = BasicSubsetF Functional

isUninhabited :: Functional -> Boolean
isUninhabited (Function _ _) = false -- due to effects
isUninhabited (TyVar _) = false -- unknown
isUninhabited (Union tys) = all isUninhabited tys
isUninhabited (Concrete (Singleton _)) = false
isUninhabited (Concrete AnyScalar) = false
isUninhabited (Concrete (ListOf ty)) = isUninhabited ty
isUninhabited (Concrete (Tupled tys)) = any isUninhabited tys

type Loc = Unit
dfLoc = mempty :: Loc

data Error
  = EExpectedFunctionAtGot Loc Functional
  | ECannotMatchAgainstVariablesAt Loc (Array (Tuple PatLoc TyVar))
  | EShadowTypeMismatchAt Loc TmVar Functional Functional

newtype Locals = Locals
  { boundVariables :: Map TmVar Functional
  }
derive instance Newtype Locals _

type Warnings = List Void

newtype Typing a = Typing (RWSE Locals Warnings Unit Error a)
derive instance newtypeTyping :: Newtype (Typing a) _
derive newtype instance semigroupTyping :: Semigroup a => Semigroup (Typing a)
derive newtype instance monoidTyping :: Monoid a => Monoid (Typing a)
derive newtype instance functorTyping :: Functor Typing
derive newtype instance applyTyping :: Apply Typing
derive newtype instance applicativeTyping :: Applicative Typing
derive newtype instance bindTyping :: Bind Typing
derive newtype instance monadTyping :: Monad Typing
derive newtype instance monadThrow :: MonadThrow Error Typing
derive newtype instance monadAsk :: MonadAsk Locals Typing
derive newtype instance monadReader :: MonadReader Locals Typing

newtype NatTrans :: forall k. (k -> Type) -> (k -> Type) -> Type
newtype NatTrans f g = NatTrans (f ~> g)
instance semigroupoidNatTrans :: Semigroupoid NatTrans where
  compose (NatTrans x) (NatTrans y) = NatTrans (\a -> x (y a))
instance categoryNatTrans :: Category NatTrans where
  identity = NatTrans \a -> a

bindValue :: TmVar -> Functional -> Local
bindValue name ty = Endo $ NatTrans do
  lift2 (*>)
    do
      const do
        Locals { boundVariables } <- ask
        for_ (Map.lookup name boundVariables) \ty0 -> do
          when (ty /= ty0) do
            throwError (EShadowTypeMismatchAt dfLoc name ty0 ty)
    do
      local $ _Newtype $
        prop (Proxy :: Proxy "boundVariables") $
          Map.insert name ty

type Local = Endo NatTrans Typing
type BindIt = TmVar -> Functional -> Local

binding :: Local -> forall a. Typing a -> Typing a
binding (Endo (NatTrans f)) x = f x

printPattern :: Pattern -> String
printPattern (Var v) = v
printPattern (Scalar s) = show s
printPattern (Vector vs) = "[" <> intercalateMap " " printPattern vs <> "]"
printPattern (Macro _fn _args) = "($)"

testPatternsResult ::
  Either Error
    { matched :: Array (Tuple Functional (Array (Tuple String Functional)))
    , unmatched :: Functional
    } ->
  String
testPatternsResult (Left err) = case err of
  EExpectedFunctionAtGot _loc ty ->
    "Expected function, got " <> printFunctional ty
  ECannotMatchAgainstVariablesAt _loc patLocTyVars ->
    "Cannot match against type variable(s) " <>
      intercalateMap ", " (\(Tuple patLoc tyVar) -> tyVar <> " (at " <> show patLoc <> ")") patLocTyVars
  EShadowTypeMismatchAt _loc var ty1 ty2 -> fold
    [ "Cannot match "
    , var
    , " against type "
    , printFunctional ty2
    , " since it is already bound with type "
    , printFunctional ty1
    ]
testPatternsResult (Right x) = fold
  [ x.matched # foldMap \m -> fold
    [ printFunctional (fst m)
    , snd m # foldMap \(Tuple v y) ->
        "\n  " <> v <> " : " <> printFunctional y
    , "\n------\n"
    ]
  , printFunctional x.unmatched
  ]

unUnion :: Functional -> Array Functional
unUnion (Union options) = options >>= unUnion
unUnion t = [t]

normalizeFunctional :: Functional -> Functional
normalizeFunctional t = fromMaybe (Union []) $ normalizeFunctional' t

normalizeFunctional' :: Functional -> Maybe Functional
normalizeFunctional' = case _ of
  Function i o -> Just $ Function (normalizeFunctional i) (normalizeFunctional o)
  Concrete (ListOf t) | Nothing <- normalizeFunctional' t -> Nothing
  Concrete (Tupled ts) -> Concrete <<< Tupled <$> traverse normalizeFunctional' ts
  Concrete c -> Just $ Concrete c
  TyVar v -> Just $ TyVar v
  Union options ->
    case Array.nubEq $ unUnion <<< normalizeFunctional =<< options of
      [] -> Nothing
      [ t ] -> Just t
      ts -> Just $ Union ts

printFunctional :: Functional -> String
printFunctional = normalizeFunctional >>> case _ of
  Function i o -> fold [ "(", printFunctional i, "->", printFunctional o, ")" ]
  Concrete c -> case c of
    Singleton s -> show s
    AnyScalar -> "$$"
    ListOf t -> "+" <> printFunctional t
    Tupled ts -> "[" <> intercalateMap " " printFunctional ts <> "]"
  TyVar v -> v
  Union [] -> "(|)"
  Union [ t ] -> printFunctional t
  Union [ Concrete (Singleton ""), Concrete AnyScalar ] -> "$"
  Union [ Concrete (Tupled []), Concrete (ListOf t) ] -> "*" <> printFunctional t
  Union options -> "(" <> intercalateMap " | " printFunctional options <> ")"

testPattern ::
  Pattern -> Functional ->
  Either Error
    { matched :: Tuple Functional (Array (Tuple String Functional))
    , unmatched :: Functional
    }
testPattern pat ty = fst $ evalRWSE (Locals { boundVariables: Map.empty }) unit $ unwrap $
  bashing pat ty bindValue \ty' -> do
    Locals { boundVariables: l } <- ask
    pure $ Tuple ty' $ Map.toUnfoldable l

testPatterns ::
  Array Pattern -> Functional ->
  Either Error
    { matched :: Array (Tuple Functional (Array (Tuple String Functional)))
    , unmatched :: Functional
    }
testPatterns pats ty =
  Array.foldl testMore (Right { matched: [], unmatched: ty }) pats
  where
  testMore (Left e) _ = Left e
  testMore (Right { matched, unmatched }) pat =
    case testPattern pat unmatched of
      Left e -> Left e
      Right m -> Right
        { matched: Array.snoc matched m.matched
        , unmatched: m.unmatched
        }

bashing :: forall a.
  Pattern -> Functional -> BindIt ->
  (Functional -> Typing a) ->
  Typing { matched :: a, unmatched :: Functional }
bashing value ascription varWithType r = case seen (bash value ascription) of
  Tuple _ (Unown vars) -> throwError (ECannotMatchAgainstVariablesAt dfLoc vars)
  Tuple _varsSeen (Known matched unmatched) -> do
    let filtered = normalizeFunctional (Union matched)
    case seen (bash value filtered) of
      Tuple _ (Unown _) -> unsafeCrashWith "bashing did not succeed again"
      Tuple varsSeen _ -> do
        let bound = foldMapWithIndex (foldMap <<< varWithType) varsSeen
        { matched: _
        , unmatched: Union (unmatched)
        } <$> binding bound (r (Union (matched)))

data Refine a
  = Unown (Options (Tuple PatLoc TyVar))
  | Known (Options a) (Options a)

derive instance functorRefine :: Functor Refine

instance semigroupRefine :: Semigroup (Refine a) where
  append (Unown a1) (Unown a2) = Unown (a1 <> a2)
  append (Known a1 b1) (Known a2 b2) = Known (a1 <> a2) (b1 <> b2)
  append r@(Unown _) (Known _ _) = r
  append (Known _ _)  r@(Unown _) = r

instance monoidRefine :: Monoid (Refine a) where
  mempty = Known mempty mempty

keep :: forall a. a -> (a -> a) -> a
keep a f = f a

-- | A result where we decide if it matches the type or not.
known :: forall a. Boolean -> a -> Refine a
known true ty = Known [ty] []
known false ty = Known [] [ty]

-- | A result where we cannot match it against the type
unknown :: forall a. TyVar -> SeeVars (Refine a)
unknown tv = ask <#> \loc -> Unown [Tuple loc tv]

-- | Collate refinement for list patterns
refinedList :: forall a. Items (Refine a) -> Either (Options (Tuple PatLoc String)) (Items (Options a))
refinedList =
  let
    p = case _ of
      Unown l -> Left l
      Known r _ -> Right r
    f = case _ of
      { left: [], right } -> Right right
      { left } -> Left (join left)
  in partitionMap p >>> f

-- | Collate refinement for tuple patterns
refineTupled :: forall a. Items (Refine a) -> Refine (Items (Options a))
refineTupled =
  let
    {-
      | ["Unown" l] => ["L" l];
      | ["Known" r1 r2] => ["R" [r1 r2]];
      !
    -}
    p = case _ of
      Unown l -> Left l
      Known r1 r2 -> Right (Tuple r1 r2)
    -- TODO[PERF]: quadratic because `Array.cons`
    slideCase ::
      Tuple (Options a) (Options a) ->
      Tuple (Options (Items (Options a))) (Items (Options a)) ->
      Tuple (Options (Items (Options a))) (Items (Options a))
    slideCase (Tuple ifMatch ifNotMatch) (Tuple genericTail afterMatchTail) =
      {-
        slideCase [ifMatch ifNotMatch] [genericTail afterMatchTail] => result:
          append ifMatch ifNotMatch => regardless
          let [
            [
              [ifMatch ...afterMatchtail]
              [ifNotMatch ...genericTail]
            ]
            [regardless ...afterMatchTail]
          ] => result
      -}
      Tuple
        do
          Array.cons
            do Array.cons ifNotMatch afterMatchTail
            do Array.cons ifMatch <$> genericTail
        do
          Array.cons (ifMatch <> ifNotMatch) afterMatchTail
    {-
      | [[] right] => ["Known" refineMatch refineOther]:
        map | [rightOption _] => [rightOption] ! right => refineMatch
      | [left _] => ["Unown" allLeftOptions]
        join left => allLeftOptions
      !
    -}
    f = case _ of
      { left: [], right } ->
        Known [map fst right] $
          Array.dropEnd 1 $
            fst (foldr slideCase (Tuple [[]] []) right)
      { left } -> Unown (join left)
  in partitionMap p >>> f

-- | We keep the location inside of patterns where we match variables.
-- | This lets us distinguish non-linear variables in the same pattern
-- | (like `["B" a a]`) from variables that got typed several times
-- | (like `[a] (: [[]] | +$)`, whence `a (: [] | $)`).
type PatLoc = List Int
type SeeVar = Tuple TmVar (Tuple PatLoc Functional)
type SeeVars = ReaderT PatLoc (WriterT (Array SeeVar) Identity)

-- | Record seeing a variable at the current position
see :: String -> Functional -> SeeVars Unit
see v ty = ask >>= \loc -> tell [Tuple v (Tuple loc ty)]

-- | Push the position in a vector pattern onto the stack
into :: forall a. Int -> SeeVars a -> SeeVars a
into idx = local (idx : _)

-- | Run a computation and state where the variables were seen
seen :: forall a. SeeVars a -> Tuple (SemigroupMap TmVar (SemigroupMap PatLoc Functional)) a
seen = flip runReaderT Nil >>> runWriter >>> case _ of
  Tuple a vars -> flip Tuple a $ vars # foldMap
    \(Tuple var (Tuple loc ty)) ->
      coerce $ Map.singleton var $ Map.singleton loc ty

-- | The main case bash for associating structural patterns with structural
-- | types.
basher :: Pattern -> SubsetWithFunctions -> SeeVars (Refine SubsetWithFunctions)
basher (Var v) ty = see v (Concrete ty) $> known true ty
basher (Scalar s) ty@(Singleton s') = pure $ known (s == s') ty
basher (Scalar s) ty@AnyScalar = pure $ known (s /= "") ty
basher (Scalar _) ty@(ListOf _) = pure $ known false ty
basher (Scalar _) ty@(Tupled _) = pure $ known false ty
basher (Vector []) ty@(ListOf _) = pure $ known false ty
basher (Vector s) (ListOf itemty) =
  refinedList <$> (traverseWithIndex into (bash <$> s <@> itemty))
    <#> case _ of
      Left unk -> Unown unk
      -- We take the refined types and tuple them up
      -- But we make no progress on the unrefined type
      Right refinedItemTypes ->
        known true (Tupled (Union <$> refinedItemTypes))
        <> known false (ListOf itemty)
basher (Vector s) ty@(Tupled items) =
  case Array.length s == Array.length items of
    true -> map (Tupled <<< map Union) <<< refineTupled <$>
      traverseWithIndex into (Array.zipWith bash s items)
    false -> pure $ known false ty
basher (Vector _) ty@(Singleton _) = pure $ known false ty
basher (Vector _) ty@AnyScalar = pure $ known false ty
basher (Macro _ _) _ = unsafeCrashWith "Unexpected Macro in basher"

-- | Apply patterns to more general types.
bash :: Pattern -> Functional -> SeeVars (Refine Functional)
bash (Var v) ty = see v ty $> known true ty
-- Handle each type in the union separately
bash pat (Union tys) = foldMap (bash pat) tys
-- `basher` is the handler for concrete types
bash pat (Concrete ty) = map Concrete <$> basher pat ty
-- Scalars and Vectors are disjoint from functions
bash _ ty@(Function _ _) = pure $ known false ty
-- We cannot match a concrete pattern against a tyvar
bash _ (TyVar tv) = unknown tv

-- instance TypeSystem Functional (Typing Unit) where
--   unFunAbs (Function dom cod) cb = cb dom cod
--   unFunAbs (NotFunction subset) _ = throwError (EExpectedFunctionAtGot dfLoc subset)

--   -- unFunApp fnExpr args resultant =
--   pattern pat expectedType didMatch didNotMatch =
--     case pat, expectedType of
--       Var name, _ -> bindValue name expectedType didMatch <> didNotMatch
--       -- asdf
--       _, _ -> throwError (EPatternMismatchAt dfLoc pat expectedType)
--   construct pat expectedType =

--   failureCase _ = pure unit
