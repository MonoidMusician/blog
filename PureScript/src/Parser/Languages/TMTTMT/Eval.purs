module Parser.Languages.TMTTMT.Eval where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Plus (empty)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..), foldMap)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.String as String
import Data.Traversable (fold, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Parser.Languages.TMTTMT.Parser (printExpr, printPattern)
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..), Var)

data Valu
  = EVar Var
  | EScalar String
  | EVector (Array Valu)
  | EClosure Ctx Expr
  | EOptions (NonEmptyArray Valu)


type Ctx =
  { locals :: Map String Valu
  , globals :: Map String Expr
  }

lookupIn :: Ctx -> String -> Eval Valu
lookupIn ctx v = case Map.lookup v ctx.locals of
  Just e -> pure e
  Nothing ->
    case Map.lookup v ctx.globals of
      Just e -> pure $ EClosure { locals: Map.empty, globals: ctx.globals } e
      Nothing -> throwError $ LookupFailure v ctx

extend :: Ctx -> String -> Valu -> Ctx
extend past v e = past { locals = Map.insert v e past.locals }

fromDeclarations :: Array Declaration -> Ctx
fromDeclarations = \decls ->
  let
    addDecl (DefineCase name _path casing) = SemigroupMap $
      Map.singleton name [casing]
    addDecl _ = mempty
    SemigroupMap ctx = Lambda <$> A.foldMap addDecl decls
  in { locals: Map.empty, globals: ctx }

data Eval a
  = Result a
  | Defer (Unit -> Eval a)
  | Error EvalError

force :: forall a. Eval a -> Either EvalError a
force (Result a) = Right a
force (Error e) = Left e
force (Defer u) = force (u unit)

limit :: forall a. Int -> Eval a -> Either EvalError a
limit n _ | n <= 0 = Left Loop
limit n (Defer u) = limit (n - 1) (u unit)
limit _ (Result a) = Right a
limit _ (Error e) = Left e

derive instance functorEval :: Functor Eval
instance applyEval :: Apply Eval where
  apply = ap
instance applicativeEval :: Applicative Eval where
  pure = Result
instance bindEval :: Bind Eval where
  bind (Result a) f = f a
  bind (Defer a) f = Defer (a >>> (_ >>= f))
  bind (Error e) _ = Error e
instance monadEval :: Monad Eval
instance monadThrowEval :: MonadThrow EvalError Eval where
  throwError = Error
-- instance monadErrorEval :: MonadError EvalError Eval where
--   catchError (Result a) _ = Result a
--   catchError ()
-- instance foldableEval :: Foldable Eval where
--   foldMap f (Result a) = f a
--   foldMap _ (Error _) = mempty
--   foldl f b (Result a) = f b a
--   foldl _ b (Error _) = b
--   foldr f b (Result a) = f a b
--   foldr _ b (Error _) = b
-- instance traversableEval :: Traversable Eval where
--   traverse f (Result a) = Result <$> f a
--   traverse f (Defer a) = Defer <<< const <$> traverse f (a unit)
--   traverse _ (Error e) = pure (Error e)
--   sequence (Result a) = Result <$> a
--   sequence (Defer a) = Defer <<< const <$> a unit
--   sequence (Error e) = pure (Error e)
instance monoidEval :: Monoid a => Monoid (Eval a) where mempty = pure mempty
instance semigroupEval :: Semigroup a => Semigroup (Eval a) where append = lift2 append

patFail :: forall a. Valu -> Valu -> Eval a
patFail pattern expr = throwError (PatternFailure (pure { pattern, expr }))

type PatternFailure =
  { pattern :: Valu
  , expr :: Valu
  }

data EvalError
  = PatternFailure (List PatternFailure)
  | LookupFailure String Ctx
  | NotAFunction Pattern
  | NotAPattern Expr
  | Stuck String
  | Loop

indentN :: Int -> String -> String
indentN _ s | not String.contains (String.Pattern "\n") s = s
indentN n s = "\n" <> s # String.replaceAll
  (String.Pattern "\n")
  (String.Replacement $ "\n" <> power " " n)

printEvalError :: EvalError -> String
printEvalError = append "Evaluation error: " <<< case _ of
  PatternFailure Nil -> "No cases"
  PatternFailure failures -> fold
    [ "No cases matched:"
    , failures # foldMap \{ pattern, expr } ->
        "\n  " <> printExpr (quote expr) <> " => " <> indentN 4 (printExpr (quote pattern))
    ]
  LookupFailure var' ctx -> fold
    [ "Variable missing from context:"
    , "\n  " <> var' <> " not in"
    , ctx.locals # foldMapWithIndex \var val ->
        "\n    " <> var -- <> " := " <> indentN 6 (printExpr val)
    ]
  NotAFunction val -> fold
    [ "Tried to apply to non-function:"
    , "\n  " <> indentN 4 (printPattern val)
    ]
  NotAPattern expr -> fold
    [ "Evaluated to non-pattern:"
    , "\n  " <> indentN 4 (printExpr expr)
    ]
  Stuck var -> fold
    [ "Stuck on variable:"
    , "\n  " <> var
    ]
  Loop -> "Potential infinite loop"

-- TODO: stuck
fallback :: forall a. (Unit -> Eval a) -> Eval a -> Eval a
fallback deferred = case _ of
  Result r -> Result r
  Defer x -> fallback deferred (x unit)
  Error (PatternFailure reasons) ->
    addFailures reasons (deferred unit)
  Error e -> Error e

addFailures :: forall a.
  List
    { expr :: Valu
    , pattern :: Valu
    }
  -> Eval a -> Eval a
addFailures reasons = case _ of
  Result r -> Result r
  Defer x -> addFailures reasons (x unit)
  Error (PatternFailure moreReasons) ->
    Error (PatternFailure (reasons <> moreReasons))
  Error e -> Error e

quote :: Valu -> Expr
quote (EVar v) = Pattern $ Var v
quote (EScalar s) = Pattern $ Scalar s
quote (EVector vs) = Pattern $ Vector $ unExpr <<< quote <$> vs
quote (EClosure ctx expr) = substitute ctx expr
quote (EOptions opts) = Lambda $ map quote opts # foldMap \opt -> case opt of
  Lambda cases -> cases
  e -> [Case (Matching [] e) []]

unExpr :: Expr -> Pattern
unExpr (Pattern p) = p
unExpr e = Macro e []

-- TODO: capture avoiding
substitute :: Ctx -> Expr -> Expr
substitute _ (Pattern (Scalar s)) = Pattern $ Scalar s
substitute ctx (Pattern (Vector vs)) = Pattern $ Vector $ vs <#> Pattern >>> substitute ctx >>> unExpr
substitute ctx (Pattern (Macro fn args)) = Pattern (Macro (substitute ctx fn) (substitute ctx <$> args))
substitute ctx (Pattern (Var v)) =
  case Map.lookup v ctx.locals of
    Nothing -> Pattern (Var v)
    Just e -> quote e
substitute ctx (Lambda cases) = Lambda $ cases <#>
  \(Case (Matching pats result) conds) -> Case
    (Matching (pats <#> Pattern >>> substitute ctx >>> unExpr) (substitute ctx result)) $
      conds <#> \(Condition fn (Calling args pat)) ->
        Condition (substitute ctx fn) $ Calling (args <#> substitute ctx) (pat # Pattern >>> substitute ctx >>> unExpr)

evalMacro :: Ctx -> Expr -> Array Expr -> Eval Valu
evalMacro ctx0 e [] = evalExpr ctx0 e
evalMacro ctx0 e allArgs = Defer \_ -> do
  argValus <- traverse (evalExpr ctx0) allArgs
  cases <- evalFun ctx0 =<< evalExpr ctx0 e
  evalCases cases argValus

evalCases :: Array (Tuple Ctx Case) -> Array Valu -> Eval Valu
evalCases cases argValus = do
  let
    matching :: Ctx -> Array Pattern -> Eval Ctx
    matching ctx patterns = do
      patterns' <- traverse (evalPattern ctx) $ A.take (A.length argValus) patterns
      A.zip patterns' argValus #
        A.foldM (\ctx' (Tuple pat expr) -> match ctx' pat expr) ctx
    tryCase :: Ctx -> Case -> Eval Valu
    tryCase ctx (Case (Matching patterns result) conds) = do
      ctx1 <- matching ctx patterns
      case A.length patterns `compare` A.length argValus of
        GT -> do
          let remaining = A.drop (A.length argValus) patterns
          pure $ EClosure ctx1 $ Lambda $ pure $ Case (Matching remaining result) conds
        EQ -> do
          ctx2 <- evals ctx1 conds
          evalExpr ctx2 result
        LT -> do
          let remaining = A.drop (A.length patterns) argValus
          ctx2 <- evals ctx1 conds
          cases' <- evalFun ctx2 =<< evalExpr ctx2 result
          evalCases cases' remaining
    trying :: _ -> Tuple Ctx Case -> _
    trying (Left r) _ = pure $ Left r
    trying (Right acc) c = fallback (\_ -> pure (Right acc)) $
      mayFinish acc =<< uncurry tryCase c

    mayFinish acc = case _ of
      r@(EClosure _ _) -> pure $ Right (acc <> [r])
      EOptions acc' -> pure $ Right (acc <> NEA.toArray acc')
      r -> Left <$> finish (acc <> [r])
    finish [e] = pure e
    finish acc = case NEA.fromArray acc of
      Nothing -> throwError $ PatternFailure empty
      Just acc' -> pure $ EOptions acc'
  either pure finish =<< A.foldM trying (Right []) cases

-- Evaluate macros within a pattern
evalPattern :: Ctx -> Pattern -> Eval Valu
evalPattern ctx = case _ of
  Var v ->
    case Map.lookup v ctx.locals of
      Just e -> pure e
      Nothing ->
        case Map.lookup v ctx.globals of
          Just e -> pure $ EClosure { locals: Map.empty, globals: ctx.globals } e
          Nothing -> pure (EVar v)
  Scalar s -> pure (EScalar s)
  Macro fn args -> evalMacro ctx fn args
  Vector exprs -> EVector <$> traverse (evalPattern ctx) exprs

-- Evaluate macros within an expression
evalExpr :: Ctx -> Expr -> Eval Valu
evalExpr ctx = case _ of
  Pattern (Var v) -> lookupIn ctx v
  Pattern (Scalar s) -> pure (EScalar s)
  Pattern (Macro fn args) -> evalMacro ctx fn args
  Pattern (Vector exprs) -> EVector <$> traverse (evalSubExpr ctx) exprs
  e@(Lambda _) -> pure $ EClosure ctx e

evalSubExpr :: Ctx -> Pattern -> Eval Valu
evalSubExpr ctx p = evalExpr ctx (Pattern p)

evalFun :: Ctx -> Valu -> Eval (Array (Tuple Ctx Case))
evalFun ctx = case _ of
  EVar v -> lookupIn ctx v >>= evalFun ctx
  EScalar s -> throwError (NotAFunction (Scalar s))
  EVector exprs -> throwError $ NotAFunction $ Vector $
    exprs <#> quote >>> unExpr
  EClosure ctx' (Lambda cases) -> pure $ Tuple ctx' <$> cases
  EClosure ctx' e -> evalExpr ctx' e >>= evalFun ctx'
  EOptions opts -> foldMap (evalFun ctx) opts

evals :: Ctx -> Array Condition -> Eval Ctx
evals = A.foldM \ctx (Condition fn (Calling args pat)) -> do
  result <- evalMacro ctx fn args
  pat' <- evalPattern ctx pat
  match ctx pat' result



match :: Ctx -> Valu -> Valu -> Eval Ctx
match ctx = case _, _ of
  EVar v, e ->
    case force (lookupIn ctx v) of
      Left _ -> pure (extend ctx v e)
      Right e' -> ctx <$ unify e e'
  EScalar s1, EScalar s2 | s1 == s2 -> pure ctx
  EVector pats, EVector exprs
    | Just patexprs <- zip pats exprs ->
      A.foldM (\ctx' (Tuple pat expr) -> match ctx' pat expr) ctx patexprs
  p, e -> patFail p e


unify :: Valu -> Valu -> Eval Valu
unify = case _, _ of
  EVar v1, EVar v2 | v1 == v2 -> pure (EVar v1)
  EScalar s1, EScalar s2 | s1 == s2 -> pure (EScalar s1)
  EVector vs1, EVector vs2 | Just vs <- zip vs1 vs2 ->
    EVector <$> traverse (uncurry unify) vs
  p1, p2 -> patFail p1 p2



zip :: forall a b. Array a -> Array b -> Maybe (Array (Tuple a b))
zip x y | A.length x /= A.length y = Nothing
zip x y = Just (A.zip x y)
