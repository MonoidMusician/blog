module Parser.Languages.TMTTMT.TypeCheck.Quantified where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (WriterT, runWriter, tell)
import Data.Array (all, any)
import Data.Array as A
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Filterable (filterMap, partitionMap)
import Data.Foldable (class Foldable, fold, foldM, foldMap, foldl, foldr, intercalate)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HeytingAlgebra (ff)
import Data.Identity (Identity)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), foldMap, foldr, (:))
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Monoid (power)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (class Traversable, for_, sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Idiolect (intercalateMap, nonEmpty)
import Parser.Languages.TMTTMT.TypeCheck (class TypeSystem, constructExpr)
import Parser.Languages.TMTTMT.TypeCheck.Structural (BasicSubsetF(..), Items, Options, Sifted(..), TmVar, TyVar, printPattern)
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Print (printQualified)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Uncurried.RWSE (RWSE, evalRWSE)

{-

  expr ::= expr (: type :)
  expr ::= [ (: overall_type :) expr* ]
  expr ::= {: assoc* :} ## strict/enumerated record, not subject to field subtyping

-}

{-
  Polymorphism and subtypes

  `stronger <: weaker`
  `subsumed <: subsumes`
  `any term of this type <:(can also be used as) a term of this type`
  `"true" <: ("true" | "false")`
  `{ x: string, y: string } <: { x: string }`

  Polymorphism increases the slipperiness:
  - Like quantifiers commute
  - Existential quantifiers are stronger before universals; they weaken when
    they slip after them:
    `exists y. forall x. f x y <: forall x. exists y. f x y`
    (since `y` can then depend on `x`)
  - Contravariant exists is covariant forall:
    `(exists y. f y) -> z = forall y. f y -> z`
    and vice versa... it just relies on occurs check for/and scoping, uh .
  - Quantifiers can slip through covariant positions of constructors...
    does strict positivity matter, or only for recursion? hard to tell, but
    maybe in some cases (or at least for typechecking in generated code)
  - Squish
    `forall x y. f x y <: forall z. f z z`
    `exists z. f z z <: exists x y. f x y`
  - You need to peek through quantifiers to see the shape of data


  E.g. Union [ (Forall _ (Function _ _)), TyVar _ ] is still a bad type for the
  same reasons that it is a bad type without a forall

  Unification-subsumption:
  Each side has:
  - Quantifier type -> variable type (skolem vs univar)
  - Quantifier dependencies??
  - Solutions

  Solutions are a bit ugly/denormalized because they are left and right, so I
  guess might as well augment with rightvar=leftvar when possible.

  `TyApp`s are weird, will ignore for now: guess we can peek at variance and
  maybe impose some variance constraints on tyvars, but yeah, not easy.
  probably don't interact nicely with `TyVar`s

  everything lifts: `k -> Type`, like forall and union and others. actually need
  to be careful with that......

  `range :: Int -> @forall (f :: @Type -> @Type | Foldable). f Int`
  `range :: Int -> @forall (f :: @Type -> @Type, Foldable f). f Int`
-}

data UniSub solution = UniSub
  -- quantifiers should be disjoint
  { quantifiers :: Map TyVar (Either Unit (Set TyVar))
  -- solutions may be overlapping
  , solutions :: Map TyVar solution
  } -- | Failed???

newtype VSifted a = VSifted { tyvarapp :: Array a }
derive newtype instance monoidVSifted :: Monoid (VSifted a)
derive newtype instance semigroupVSifted :: Semigroup (VSifted a)
derive instance newtypeVSifted :: Newtype (VSifted a) _
derive instance functorVSifted :: Functor VSifted
derive instance foldableVSifted :: Foldable VSifted
derive instance traversableVSifted :: Traversable VSifted
type QSifted = Sifted VSifted
type QSiftQ = QSifted
  { quantifiers :: List { var :: TyVar, exists :: Boolean }
  , core :: Quantified
  }

-- | It is important to be able to sift out each type of case (e.g. arrays and
-- | scalars and functions, mainly) after seeing through all of the quantifiers.
-- | This only sees through one layer, of course, and it does not canonicalize
-- | quantifiers, only records them in a faithful order.
sift :: Quantified -> QSiftQ
sift = case _ of
  Concrete c -> case c of
    Singleton s -> mk _ { singleton = Set.singleton s }
    AnyScalar -> mk _ { anyScalar = Just unit }
    ListOf t -> mk _ { listOf = Just $ unq t }
    Tupled ts -> mk _
      { tupled =
          SemigroupMap $ Map.singleton (Array.length ts) $
            SemigroupMap $ Map.fromFoldableWithIndex $ map unq ts
      }
  Function i o -> mk _ { function = Array.singleton (Tuple (unq i) (unq o)) }
  TyVarApp v ts -> mk _ { other = VSifted { tyvarapp: [unq (TyVarApp v ts)] } }
  Union ts -> foldMap sift ts
  Forall v t -> q false v $ sift t
  Exists v t -> q true v $ sift t
  where
  unq = { quantifiers: mempty, core: _ }
  mk f = Sifted $ f
    { singleton: Set.empty
    , anyScalar: Nothing
    , listOf: Nothing
    , tupled: SemigroupMap Map.empty
    , function: []
    , other: VSifted { tyvarapp: [] }
    }
  q exists var = map \{ quantifiers, core } ->
    { quantifiers: List.Cons { var, exists } quantifiers, core }

-- | Similarly, we can join it back up again. TODO: detect shared quantifiers?
unsift :: QSiftQ -> Quantified
unsift (Sifted r) = fold
  [ foldMap (Concrete <<< Singleton) r.singleton
  , foldMap (const (Concrete AnyScalar)) r.anyScalar
  , foldMap (Concrete <<< ListOf <<< q) r.listOf
  , foldMap (Concrete <<< Tupled <<< map q) $ fromTupled r.tupled
  , foldMap (\(Tuple i o) -> Function (q i) (q o)) r.function
  -- , r.other
  ]
  where
  q { quantifiers, core } =
    foldl (\t { var, exists } -> (if exists then Exists else Forall) var t) core quantifiers
  fromTupled = map $ map snd <<< Map.toUnfoldable <<< unwrap

--------------------------------------------------------------------------------

-- | Compare two parts of types to determine their overlap versus what remains
-- | as excess from the left and right.
type Comparer t = Pair t -> Compared (Maybe t)

newtype Compared t = Compared
  { left :: t
  , both :: t
  , right :: t
  }
derive instance functorCompared :: Functor Compared

compareSet :: forall t. Ord t => Comparer (Set t)
compareSet (Pair l r) = Compared
  { left: nonEmpty $ Set.difference l r
  , both: nonEmpty $ Set.intersection l r
  , right: nonEmpty $ Set.difference r l
  }

compareUnit :: Comparer Unit
compareUnit _ = Compared { left: Nothing, both: Just unit, right: Nothing }

compareMaybe :: forall t. Comparer t -> Comparer (Maybe t)
compareMaybe cmp (Pair (Just l) (Just r)) =
  Just <$> cmp (Pair l r)
compareMaybe _ (Pair l r) = Compared
  { left: Just <$> l
  , both: Nothing
  , right: Just <$> r
  }

compareMap :: forall k t. Ord k => Comparer t -> Comparer (Map k t)
compareMap cmp (Pair l r) = Compared
  { left: nonmt $ fromCompared _.left `Map.union` Map.difference l r
  , both: nonmt $ fromCompared _.both
  , right: nonmt $ fromCompared _.right `Map.union` Map.difference r l
  }
  where
  nonmt m = if Map.isEmpty m then Nothing else Just m
  compared :: Map k { left :: Maybe t, both :: Maybe t, right :: Maybe t }
  compared = ado
    lv <- l
    rv <- r
    in let Compared c = cmp (Pair lv rv) in c
  fromCompared f = filterMap f compared

compareSemigroupMap :: forall k t. Ord k => Comparer t -> Comparer (SemigroupMap k t)
compareSemigroupMap = coerce (compareMap :: Comparer t -> Comparer (Map k t))

compareTupled :: forall t. t -> Comparer t -> Comparer (SemigroupMap Int (SemigroupMap Int t))
compareTupled neutral cmp = compareSemigroupMap \(Pair l r) -> do
  -- We need to keep the `SemigroupMap Int t` padded to the same length
  -- (Really the type should be `DependentMap Int (\k -> Vector k t)`)
  let
    padding = neutral <$ void l <> void r
    padBack Nothing = Nothing
    padBack (Just result) = Just $ SemigroupMap $
      unwrap result `Map.union` unwrap padding
  padBack <$> compareSemigroupMap cmp (Pair l r)

compareFunctions :: forall t. Monoid t => t -> Comparer t -> Comparer (Array (Tuple t t))
compareFunctions neutral cmp =
  map (unionFunctions' neutral cmp) >>> case _ of
    Pair (Just (Tuple i o)) (Just (Tuple j u)) ->
      let
        Compared ij = cmp (Pair i j)
        Compared ou = cmp (Pair o u)
      -- intersection `(i -> o) & (j -> u)` and differences
      in Compared
        -- given that `(i -> o) = (i & j -> o) & (i \ j -> o)`,
        -- then `(i -> o) \ ((i -> o) & (j -> u)) = (i & j -> o \ u)`
        { left: map A.singleton $ mkfn ij.both ou.left
        -- `(i -> o) & (j -> u) = (i & j -> o & u) & (i \ j -> o) & (j \ i -> u)`
        , both: nonmt $ A.catMaybes
          [ mkfn ij.both ou.both
          , mkfn ij.left (Just o)
          , mkfn ij.right (Just u)
          ]
        , right: map A.singleton $ mkfn ij.both ou.right
        }
    _ -> Compared { left: Nothing, both: Nothing, right: Nothing }
  where
  nonmt m = if Array.null m then Nothing else Just m
  mkfn Nothing o = Just (Tuple neutral (fromMaybe neutral o))
  mkfn (Just i) (Just o) = Just (Tuple i o)
  mkfn (Just _) Nothing = Nothing

compareQSifted :: forall t. Monoid t => t -> Comparer t -> Comparer (QSifted t)
compareQSifted neutral cmp (Pair (Sifted l) (Sifted r)) = Compared
  { left: nonmt $ Sifted
    { singleton: singleton.left
    , anyScalar: anyScalar.left
    , listOf: listOf.left
    , tupled: tupled.left
    , function: function.left
    , other: VSifted { tyvarapp: tyvarapp.left }
    }
  , both: nonmt $ Sifted
    { singleton: singleton.both
    , anyScalar: anyScalar.both
    , listOf: listOf.both
    , tupled: tupled.both
    , function: function.both
    , other: VSifted { tyvarapp: tyvarapp.both }
    }
  , right: nonmt $ Sifted
    { singleton: singleton.right
    , anyScalar: anyScalar.right
    , listOf: listOf.right
    , tupled: tupled.right
    , function: function.right
    , other: VSifted { tyvarapp: tyvarapp.right }
    }
  }
  where
  nonmt (Sifted r)
    | Set.isEmpty r.singleton
    , isNothing r.anyScalar
    , isNothing r.listOf
    , Map.isEmpty (unwrap r.tupled)
    , Array.null r.function
    , Array.null (unwrap r.other).tyvarapp = Nothing
  nonmt r = Just r
  Compared singleton = map fold $ compareSet (Pair l.singleton r.singleton)
  Compared anyScalar = map join $ compareMaybe compareUnit (Pair l.anyScalar r.anyScalar)
  Compared listOf = map join $ compareMaybe cmp (Pair l.listOf r.listOf)
  Compared tupled = map (fromMaybe (SemigroupMap Map.empty)) $
    compareTupled neutral cmp (Pair l.tupled r.tupled)
  Compared function = map fold $ compareFunctions neutral cmp (Pair l.function r.function)
  Compared tyvarapp = map (fold :: Maybe _ -> Array _) $ ?help (Pair (unwrap l.other).tyvarapp (unwrap r.other).tyvarapp)

-- intersectQuantified :: NonEmptyArray Quantified -> Quantified
-- intersectQuantified = map sift >>> shallow >>> deep
--   where
--   shallow = NEA.foldl1 \l r ->
--     let Compared c = compareQSifted mempty ?help (join Pair <$> (Pair l r)) in c.both
--   deep = ?help

unionFunctions' :: forall t. Monoid t => t -> Comparer t -> Array (Tuple t t) -> Maybe (Tuple t t)
unionFunctions' neutral cmp = NEA.fromArray >>> map \fns -> do
  Tuple (fromMaybe neutral $ NEA.foldl1 cmpIntersect $ Just <<< fst <$> fns) (foldMap snd fns)
  where
  cmpIntersect = map join <<< lift2 \l r ->
    let Compared c = cmp (Pair l r) in c.both

--------------------------------------------------------------------------------

data UniOrSub = Unify | LSub | RSub

-- unisubSifted :: UniOrSub -> Pair QSiftQ -> Pair (UniSub (QSiftQ Quantified))
-- unisubSifted dir (Pair l r) =
--   unisubSingleton

-- unisubInner :: UniOrSub -> Pair Quantified -> Pair (UniSub Quantified)
-- unisubInner _ = map sift >>>

--
-- unisubInner :: UniOrSub -> Quantified -> Quantified -> Pair (UniSub Quantified)
-- unisub :: UniOrSub -> Quantified -> Quantified -> Pair Quantified
--
-- checkUniSubs :: Pair (UniSub Quantified) -> Either Error (Pair (UniSub Quantified))
-- -- transitivized:
-- -- - skolemsL :: SemigroupMap LTyVar (Set RTyVar) ~> [(Set LTyVar, Set RTyVar)] disjoint
-- -- - skolemsR
-- -- - univarsL :: Map TyVar solution ~> transitivized, check for cycles and skolem escapes and such

data Quantified
  = Function Quantified Quantified
  | Concrete (BasicSubsetF Quantified)
  | TyVarApp TyVar (Array Quantified)
  | Union (Options Quantified)
  | Forall TyVar Quantified
  | Exists TyVar Quantified

derive instance eqQuantified :: Eq Quantified

instance monoidQuantified :: Monoid Quantified where
  mempty = Union []
instance semigroupQuantified :: Semigroup Quantified where
  append (Union []) v = v
  append v (Union []) = v
  append (Union v1s) (Union v2s) = Union (v1s <> v2s)

  -- Keep flattening, to keep it associative
  append v1 (Union v2s) = Union ([v1] <> v2s)
  append (Union v1s) v2 = Union (v1s <> [v2])

  -- Finally, if none of the above cases apply,
  -- we wrap it up in a two-element array:
  append v1 v2 = Union [v1, v2]

type SubsetsWithQuantifiers = Options SubsetWithQuantifiers
type SubsetWithQuantifiers = BasicSubsetF Quantified

isUninhabited :: Quantified -> Boolean
isUninhabited (Function _ _) = false -- due to effects
isUninhabited (TyVarApp _ _) = false -- unknown
isUninhabited (Union tys) = all isUninhabited tys
isUninhabited (Concrete (Singleton _)) = false
isUninhabited (Concrete AnyScalar) = false
isUninhabited (Concrete (ListOf ty)) = isUninhabited ty
isUninhabited (Concrete (Tupled tys)) = any isUninhabited tys
isUninhabited (Forall _ ty) = isUninhabited ty
isUninhabited (Exists _ ty) = isUninhabited ty

type Loc = Unit
dfLoc = mempty :: Loc

data Error
  = EExpectedFunctionAtGot Loc Quantified
  | ECannotMatchAgainstVariablesAt Loc (Array (Tuple PatLoc TyVar))
  | EShadowTypeMismatchAt Loc TmVar Quantified Quantified
  | EVariableTypeMismatchAt Loc TmVar Quantified Quantified
  | EPatternNotSubtypeAt Loc Pattern Quantified
  | EUnknownVariableAt Loc TmVar
  | EFunctionMustBeVariableAt Loc
  | ECannotSynthesizeLambdaAt Loc

newtype Locals = Locals
  { boundVariables :: Map TmVar Quantified
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

-- bindValue :: TmVar -> Quantified -> Local
-- bindValue name ty = Endo $ NatTrans do
--   lift2 (*>)
--     do
--       const do
--         Locals { boundVariables } <- ask
--         for_ (Map.lookup name boundVariables) \ty0 -> do
--           when (ty /= ty0) do
--             throwError (EShadowTypeMismatchAt dfLoc name ty0 ty)
--     do
--       local $ _Newtype $
--         prop (Proxy :: Proxy "boundVariables") $
--           Map.insert name ty

-- assertVarSubtype :: TmVar -> Quantified -> Local
-- assertVarSubtype name ty = Endo $ NatTrans do
--   lift2 (*>)
--     do
--       const do
--         Locals { boundVariables } <- ask
--         case Map.lookup name boundVariables of
--           Just ty0 ->
--             when (not isSubtype ty0 ty) do
--               throwError (EVariableTypeMismatchAt dfLoc name ty0 ty)
--           Nothing ->
--             throwError $ EUnknownVariableAt dfLoc name
--     do identity

type Local = Endo NatTrans Typing
type BindIt = TmVar -> Quantified -> Local

binding :: Local -> forall a. Typing a -> Typing a
binding (Endo (NatTrans f)) x = f x

testPatternsResult ::
  Either Error
    { matched :: Array (Tuple (Tuple Pattern Quantified) (Array (Tuple String Quantified)))
    , unmatched :: Quantified
    } ->
  String
testPatternsResult (Left err) = "Typechecking error: " <> case err of
  EExpectedFunctionAtGot _loc ty ->
    "Expected function, got " <> printQuantified ty
  ECannotMatchAgainstVariablesAt _loc patLocTyVars ->
    "Cannot match against type variable(s) " <>
      intercalateMap ", " (\(Tuple patLoc tyVar) -> tyVar <> " (at " <> show patLoc <> ")") patLocTyVars
  EShadowTypeMismatchAt _loc var ty1 ty2 -> fold
    [ "Cannot match "
    , var
    , " against type "
    , printQuantified ty2
    , " since it is already bound with type "
    , printQuantified ty1
    ]
  EVariableTypeMismatchAt _loc var ty1 ty2 -> fold
    [ "Variable "
    , var
    , " has type "
    , printQuantified ty1
    , " but was used with expected type "
    , printQuantified ty2
    ]
  EPatternNotSubtypeAt _loc pat ty -> fold
    [ "Pattern "
    , printPattern pat
    , " does not belong to type "
    , printQuantified ty
    ]
  EUnknownVariableAt _loc var -> fold [ "Unknown variable ", printPattern (Var var) ]
  EFunctionMustBeVariableAt _loc -> fold [ "Functions must be variables, sorry." ]
  ECannotSynthesizeLambdaAt _loc -> fold [ "Cannot synthesize the type of lambdas." ]
testPatternsResult (Right x) = fold
  [ x.matched # foldMap \(Tuple (Tuple pat ty) vars) -> fold
    [ printPattern pat
    , " : "
    , printQuantified ty
    , vars # foldMap \(Tuple v y) ->
        "\n  " <> v <> " : " <> printQuantified y
    , "\n------\n"
    ]
  , printQuantified x.unmatched
  ]

unUnion :: Quantified -> Array Quantified
unUnion (Union options) = options >>= unUnion
unUnion t = [t]

normalizeQuantified :: Quantified -> Quantified
normalizeQuantified t = fromMaybe (Union []) $ normalizeQuantified' t

normalizeQuantified' :: Quantified -> Maybe Quantified
normalizeQuantified' = case _ of
  Function i o -> Just $ Function (normalizeQuantified i) (normalizeQuantified o)
  Concrete (ListOf t) | Nothing <- normalizeQuantified' t -> Nothing
  Concrete (Tupled ts) -> Concrete <<< Tupled <$> traverse normalizeQuantified' ts
  Concrete c -> Just $ Concrete c
  Forall v t -> Forall v <$> normalizeQuantified' t
  Exists v t -> Exists v <$> normalizeQuantified' t
  TyVarApp v ts -> TyVarApp v <$> traverse normalizeQuantified' ts
  Union options ->
    case Array.nubEq $ unUnion <<< normalizeQuantified =<< options of
      [] -> Nothing
      [ t ] -> Just t
      ts -> Just $ Union ts

printQuantified :: Quantified -> String
printQuantified = normalizeQuantified >>> case _ of
  Function i o -> fold [ "(", printQuantified i, " -> ", printQuantified o, ")" ]
  Concrete c -> case c of
    Singleton s -> show s
    AnyScalar -> "$$"
    ListOf t -> "+" <> printQuantified t
    Tupled ts -> "[" <> intercalateMap " " printQuantified ts <> "]"
  TyVarApp v ts -> "(" <> intercalate " " ([ v ] <> map printQuantified ts) <> ")"
  Forall v t -> "(" <> ("@" <> v <> ". " <> printQuantified t) <> ")"
  Exists v t -> "(" <> ("&" <> v <> ". " <> printQuantified t) <> ")"
  Union [] -> "(|)"
  Union [ t ] -> printQuantified t
  Union [ Concrete (Singleton ""), Concrete AnyScalar ] -> "$"
  Union [ Concrete (Tupled []), Concrete (ListOf t) ] -> "*" <> printQuantified t
  Union options -> "(" <> intercalateMap " | " printQuantified options <> ")"

-- testPattern ::
--   Pattern -> Quantified ->
--   Either Error
--     { matched :: Tuple Quantified (Array (Tuple String Quantified))
--     , unmatched :: Quantified
--     }
-- testPattern pat ty = fst $ evalRWSE (Locals { boundVariables: Map.empty }) unit $ unwrap $
--   bashing pat ty bindValue \ty' -> do
--     Locals { boundVariables: l } <- ask
--     pure $ Tuple ty' $ Map.toUnfoldable l

-- testPatterns ::
--   Array Pattern -> Quantified ->
--   Either Error
--     { matched :: Array (Tuple (Tuple Pattern Quantified) (Array (Tuple String Quantified)))
--     , unmatched :: Quantified
--     }
-- testPatterns pats ty =
--   Array.foldM testMore { matched: [], unmatched: ty } pats
--   where
--   testMore { matched, unmatched } pat =
--     testPattern pat unmatched <#> \m ->
--       { matched: Array.snoc matched (lmap (Tuple pat) m.matched)
--       , unmatched: m.unmatched
--       }

-- run :: forall a. Map String Quantified -> Typing a -> Either Error a
-- run boundVariables = fst <<< evalRWSE (Locals { boundVariables }) unit <<< unwrap

-- testExprs ::
--   Array (Either (Tuple String Quantified) Declaration) ->
--   Either Error (Map String { ty :: Quantified, def :: Expr })
-- testExprs decls =
--   fst $ evalRWSE (Locals { boundVariables }) unit $ unwrap $
--     defs <$ for_ defs \{ ty, def } -> do
--       constructExpr def ty
--   where
--   byName = Map.fromFoldableWith (<>) $ decls # filterMap case _ of
--     Left (Tuple name ty) -> Just $ Tuple name { tys: [ty], cases: [] }
--     Right (DefineCase name _path value) -> Just $ Tuple name { tys: [], cases: [value] }
--     Right _ -> Nothing
--   defs = byName # filterMap case _ of
--     { tys: [ty], cases } ->
--       Just { ty, def: Lambda cases }
--     _ -> Nothing
--   boundVariables = defs <#> _.ty

-- bashing :: forall a.
--   Pattern -> Quantified -> BindIt ->
--   (Quantified -> Typing a) ->
--   Typing { matched :: a, unmatched :: Quantified }
-- bashing value ascription varWithType r = case seen (bash value ascription) of
--   Tuple _ (Unown vars) -> throwError (ECannotMatchAgainstVariablesAt dfLoc vars)
--   Tuple _varsSeen (Known matched unmatched) -> do
--     let filtered = normalizeQuantified (Union matched)
--     case seen (bash value filtered) of
--       Tuple _ (Unown _) -> unsafeCrashWith "bashing did not succeed again"
--       Tuple varsSeen _ -> do
--         let bound = foldMapWithIndex (foldMap <<< varWithType) varsSeen
--         { matched: _
--         , unmatched: Union (unmatched)
--         } <$> binding bound (r (Union (matched)))

data Refine a
  = Unown (Options (Tuple PatLoc TyVar))
  | Known (Options a) (Options a)

derive instance functorRefine :: Functor Refine

-- instance semigroupRefine :: Semigroup (Refine a) where
--   append (Unown a1) (Unown a2) = Unown (a1 <> a2)
--   append (Known a1 b1) (Known a2 b2) = Known (a1 <> a2) (b1 <> b2)
--   append r@(Unown _) (Known _ _) = r
--   append (Known _ _)  r@(Unown _) = r

-- instance monoidRefine :: Monoid (Refine a) where
--   mempty = Known mempty mempty

-- keep :: forall a. a -> (a -> a) -> a
-- keep a f = f a

-- -- | A result where we decide if it matches the type or not.
-- known :: forall a. Boolean -> a -> Refine a
-- known true ty = Known [ty] []
-- known false ty = Known [] [ty]

-- -- | A result where we cannot match it against the type
-- unknown :: forall a. TyVar -> SeeVars (Refine a)
-- unknown tv = ask <#> \loc -> Unown [Tuple loc tv]

-- -- | Collate refinement for list patterns
-- refinedList :: forall a. Items (Refine a) -> Either (Options (Tuple PatLoc String)) (Items (Options a))
-- refinedList =
--   let
--     p = case _ of
--       Unown l -> Left l
--       Known r _ -> Right r
--     f = case _ of
--       { left: [], right } -> Right right
--       { left } -> Left (join left)
--   in partitionMap p >>> f

-- -- | Collate refinement for tuple patterns
-- refineTupled :: forall a. Items (Refine a) -> Refine (Items (Options a))
-- refineTupled =
--   let
--     {-
--       | ["Unown" l] => ["L" l];
--       | ["Known" r1 r2] => ["R" [r1 r2]];
--       !
--     -}
--     p = case _ of
--       Unown l -> Left l
--       Known r1 r2 -> Right (Tuple r1 r2)
--     -- TODO[PERF]: quadratic because `Array.cons`
--     slideCase ::
--       Tuple (Options a) (Options a) ->
--       Tuple (Options (Items (Options a))) (Items (Options a)) ->
--       Tuple (Options (Items (Options a))) (Items (Options a))
--     slideCase (Tuple ifMatch ifNotMatch) (Tuple genericTail afterMatchTail) =
--       {-
--         slideCase [ifMatch ifNotMatch] [genericTail afterMatchTail] => result:
--           append ifMatch ifNotMatch => regardless
--           let [
--             [
--               [ifMatch ...afterMatchtail]
--               [ifNotMatch ...genericTail]
--             ]
--             [regardless ...afterMatchTail]
--           ] => result
--       -}
--       Tuple
--         do
--           Array.cons
--             do Array.cons ifNotMatch afterMatchTail
--             do Array.cons ifMatch <$> genericTail
--         do
--           Array.cons (ifMatch <> ifNotMatch) afterMatchTail
--     {-
--       | [[] right] => ["Known" refineMatch refineOther]:
--         map | [rightOption _] => [rightOption] ! right => refineMatch
--       | [left _] => ["Unown" allLeftOptions]
--         join left => allLeftOptions
--       !
--     -}
--     f = case _ of
--       { left: [], right } ->
--         Known [map fst right] $
--           Array.dropEnd 1 $
--             fst (foldr slideCase (Tuple [[]] []) right)
--       { left } -> Unown (join left)
--   in partitionMap p >>> f

-- | We keep the location inside of patterns where we match variables.
-- | This lets us distinguish non-linear variables in the same pattern
-- | (like `["B" a a]`) from variables that got typed several times
-- | (like `[a] (: [[]] | +$ :)`, whence `a (: [] | $ :)`).
type PatLoc = List Int
type SeeVar = Tuple TmVar (Tuple PatLoc Quantified)
type SeeVars = ReaderT PatLoc (WriterT (Array SeeVar) Identity)

-- | Record seeing a variable at the current position
see :: String -> Quantified -> SeeVars Unit
see v ty = ask >>= \loc -> tell [Tuple v (Tuple loc ty)]

-- | Push the position in a vector pattern onto the stack
into :: forall a. Int -> SeeVars a -> SeeVars a
into idx = local (idx : _)

-- | Run a computation and state where the variables were seen
seen :: forall a. SeeVars a -> Tuple (SemigroupMap TmVar (SemigroupMap PatLoc Quantified)) a
seen = flip runReaderT Nil >>> runWriter >>> case _ of
  Tuple a vars -> flip Tuple a $ vars # foldMap
    \(Tuple var (Tuple loc ty)) ->
      coerce $ Map.singleton var $ Map.singleton loc ty

-- -- | The main case bash for associating structural patterns with structural
-- -- | types.
-- basher :: Pattern -> SubsetWithQuantifiers -> SeeVars (Refine SubsetWithQuantifiers)
-- basher (Var v) ty = see v (Concrete ty) $> known true ty
-- basher (Scalar s) ty@(Singleton s') = pure $ known (s == s') ty
-- basher (Scalar s) ty@AnyScalar = pure $ known (s /= "") ty
-- basher (Scalar _) ty@(ListOf _) = pure $ known false ty
-- basher (Scalar _) ty@(Tupled _) = pure $ known false ty
-- basher (Vector []) ty@(ListOf _) = pure $ known false ty
-- basher (Vector s) (ListOf itemty) =
--   refinedList <$> (traverseWithIndex into (bash <$> s <@> itemty))
--     <#> case _ of
--       Left unk -> Unown unk
--       -- We take the refined types and tuple them up
--       -- But we make no progress on the unrefined type
--       Right refinedItemTypes ->
--         known true (Tupled (Union <$> refinedItemTypes))
--         <> known false (ListOf itemty)
-- basher (Vector s) ty@(Tupled items) =
--   case Array.length s == Array.length items of
--     true -> map (Tupled <<< map Union) <<< refineTupled <$>
--       traverseWithIndex into (Array.zipWith bash s items)
--     false -> pure $ known false ty
-- basher (Vector _) ty@(Singleton _) = pure $ known false ty
-- basher (Vector _) ty@AnyScalar = pure $ known false ty
-- basher (Macro _ _) _ = unsafeCrashWith "Unexpected Macro in basher"

-- -- | Apply patterns to more general types.
-- bash :: Pattern -> Quantified -> SeeVars (Refine Quantified)
-- bash (Var v) ty = see v ty $> known true ty
-- -- Handle each type in the union separately
-- bash pat (Union tys) = foldMap (bash pat) tys
-- -- `basher` is the handler for concrete types
-- bash pat (Concrete ty) = map Concrete <$> basher pat ty
-- -- Scalars and Vectors are disjoint from functions
-- bash _ ty@(Function _ _) = pure $ known false ty
-- -- We cannot match a concrete pattern against a tyvar
-- bash _ (TyVar tv) = unknown tv

-- data VarsSolve
--   = NoSolve
--   | Solve (Map TyVar TyVar)

-- instance HeytingAlgebra VarsSolve where
--   tt = Solve Map.empty
--   ff = NoSolve
--   conj NoSolve _ = NoSolve
--   conj _ NoSolve = NoSolve
--   conj (Solve l) (Solve r) =
--     let
--       lr = l <> r
--       rl = r <> l
--     in if lr == rl then Solve lr else NoSolve
--   disj NoSolve s = s
--   disj s NoSolve = s
--   disj (Solve l) (Solve r) | l == r = Solve l -- TODO? good enough for now?
--   disj _ _ = NoSolve

--   -- Dunno
--   not (Solve _) = NoSolve
--   not NoSolve = Solve Map.empty
--   implies x y = y || not x

-- tf :: forall h. HeytingAlgebra h => Boolean -> h
-- tf = ?help

-- isSubtype :: Quantified -> Quantified -> Boolean
-- isSubtype (Union xs) y = all (isSubtype <@> y) xs
-- isSubtype x@(TyVar v) y = case y of
--   Union ys -> any (isSubtype x) ys
--   TyVar v' -> tf (v' == v)
--   Function _ _ -> ff
--   Concrete _ -> ff
-- isSubtype x@(Function i o) y = case y of
--   Union ys -> any (isSubtype x) ys
--   Function i' o' -> isSubtype o o' && isSubtype i' i
--   Concrete _ -> ff
--   TyVar _ -> ff
-- isSubtype x@(Concrete u) y = case y of
--   Union ys -> any (isSubtype x) ys
--   Concrete v -> isConcreteSubtype u v
--   Function _ _ -> ff
--   TyVar _ -> ff

-- isConcreteSubtype :: BasicSubsetF Quantified -> BasicSubsetF Quantified -> Boolean
-- isConcreteSubtype AnyScalar AnyScalar = true
-- isConcreteSubtype (Singleton s) AnyScalar = s /= ""
-- isConcreteSubtype (Singleton s) (Singleton s') = s == s'
-- isConcreteSubtype AnyScalar (Singleton _) = false
-- isConcreteSubtype AnyScalar _ = false
-- isConcreteSubtype _ AnyScalar = false
-- isConcreteSubtype (Singleton _) _ = false
-- isConcreteSubtype _ (Singleton _) = false
-- isConcreteSubtype (ListOf x) (ListOf y) = isSubtype x y
-- isConcreteSubtype (Tupled xs) (ListOf y)
--   | not Array.null xs = all (isSubtype <@> y) xs
-- isConcreteSubtype (Tupled xs) (Tupled ys)
--   | Array.length xs == Array.length ys =
--     all (uncurry isSubtype) (Array.zip xs ys)
-- isConcreteSubtype (ListOf _) _ = false
-- isConcreteSubtype (Tupled _) _ = false

-- instance TypeSystem Quantified (Typing Unit) where
--   unFunAbs :: Quantified -> (Quantified -> Quantified -> Typing Unit) -> Typing Unit
--   unFunAbs (Function dom cod) cb = cb dom cod
--   unFunAbs ty _ = throwError (EExpectedFunctionAtGot dfLoc ty)

--   unFunApp :: Expr -> Array (Quantified -> Typing Unit) -> (Quantified -> Typing Unit) -> Typing Unit
--   unFunApp fn args result = synFunApp fn args >>= result

--   pattern :: Pattern -> Quantified -> Typing Unit -> (Quantified -> Typing Unit) -> Typing Unit
--   pattern pat ty ifMatch ifNoMatch =
--     bashing pat ty bindValue (checkNotVoid pat ty ifMatch) >>= _.unmatched >>> ifNoMatch

--   construct :: Pattern -> Quantified -> Typing Unit
--   construct (Macro fn args) tyC = do
--     tyS <- synFunApp fn (constructExpr <$> args)
--     when (not isSubtype tyS tyC) do
--       throwError $ EPatternNotSubtypeAt dfLoc (Macro fn args) tyC
--   construct pat ty =
--     void $ bashing pat ty assertVarSubtype (checkNotVoid pat ty (pure unit))

--   failureCase :: Quantified -> Typing Unit
--   failureCase _ = pure unit

-- checkNotVoid :: Pattern -> Quantified -> Typing Unit -> Quantified -> Typing Unit
-- checkNotVoid pat ty ret v =
--   case isUninhabited v of
--     true -> throwError $ EPatternNotSubtypeAt dfLoc pat ty
--     false -> ret


-- synFunApp :: Expr -> Array (Quantified -> Typing Unit) -> Typing Quantified
-- synFunApp (Pattern (Var name)) args = do
--   Locals { boundVariables } <- ask
--   case Map.lookup name boundVariables of
--     Nothing -> throwError $ EUnknownVariableAt dfLoc name
--     Just funTy0 -> foldM peelArg funTy0 args
--   where
--   peelArg funTy arg = case funTy of
--     Function dom cod -> cod <$ arg dom
--     _ -> throwError (EExpectedFunctionAtGot dfLoc funTy)
-- synFunApp (Pattern pat) [] = synPattern pat
-- synFunApp _ _ = throwError $ EFunctionMustBeVariableAt dfLoc


-- synPattern :: Pattern -> Typing Quantified
-- synPattern (Var name) = do
--   Locals { boundVariables } <- ask
--   case Map.lookup name boundVariables of
--     Nothing -> throwError $ EUnknownVariableAt dfLoc name
--     Just ty -> pure ty
-- synPattern (Scalar v) = pure $ Concrete $ Singleton v
-- synPattern (Vector pats) = Concrete <<< Tupled <$> traverse synPattern pats
-- -- TODO: effects
-- synPattern (Macro fn args) = synFunApp fn (constructExpr <$> args)

-- synExpr :: Expr -> Typing Quantified
-- synExpr (Pattern pat) = synPattern pat
-- synExpr (Lambda _) = throwError $ ECannotSynthesizeLambdaAt dfLoc

