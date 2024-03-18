module Parser.Languages.TMTTMT.Eval where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Plus (empty)
import Data.Array (fromFoldable)
import Data.Array as A
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..), foldMap)
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (fold, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Debug (spy)
import Parser.Languages.TMTTMT.Parser (printExpr, printPattern)
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..))

type Ctx =
  { values :: Map String Expr
  , topLevel :: Set String
  }

lookupIn :: Ctx -> String -> Eval Expr
lookupIn ctx@{ values } v = case Map.lookup v values of
  Nothing -> throwError $ LookupFailure v ctx
  Just e -> pure e

extend :: Ctx -> String -> Expr -> Ctx
extend past v e = past { values = Map.insert v e past.values }

narrow :: Ctx -> String -> Ctx
narrow ctx v =
  if Set.member v ctx.topLevel then revert ctx else ctx

revert :: Ctx -> Ctx
revert ctx = ctx { values = Map.intersection ctx.values (Set.toMap ctx.topLevel) }

fromDeclarations :: Array Declaration -> Ctx
fromDeclarations = \decls ->
  let
    addDecl (DefineCase name _path casing) = SemigroupMap $
      Map.singleton name [casing]
    addDecl _ = mempty
    SemigroupMap ctx = Lambda <$> A.foldMap addDecl decls
  in { values: ctx, topLevel: Map.keys ctx }

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
instance mondEval :: Monad Eval
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

patFail :: forall a. Expr -> Expr -> Eval a
patFail pattern expr = throwError (PatternFailure (pure { pattern, expr }))

type PatternFailure =
  { pattern :: Expr
  , expr :: Expr
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
        "\n  " <> printExpr expr <> " => " <> indentN 4 (printExpr pattern)
    ]
  LookupFailure var' ctx -> fold
    [ "Variable missing from context:"
    , "\n  " <> var' <> " not in"
    , ctx.values # foldMapWithIndex \var val ->
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

sigil :: Condition
sigil = Condition (Pattern (Var "")) (Calling [] (Var ""))

isSigil :: Condition -> Boolean
isSigil (Condition (Pattern (Var "")) (Calling [] (Var ""))) = true
isSigil _ = false

unSigil :: Tuple Ctx (Array Condition) -> Tuple Ctx (Array Condition)
unSigil (Tuple ctx conds) = case Array.uncons conds of
  Just { head: c, tail: cs } | isSigil c -> Tuple (revert ctx) cs
  _ -> Tuple ctx conds

-- TODO: stuck
fallback :: forall a. (Unit -> Eval a) -> Eval a -> Eval a
fallback deferred = case _ of
  Result r -> Result r
  Defer x -> fallback deferred (x unit)
  Error (PatternFailure reasons) ->
    addFailures reasons (deferred unit)
  Error e -> Error e

addFailures :: forall a216.
  List
    { expr :: Expr
    , pattern :: Expr
    }
  -> Eval a216 -> Eval a216
addFailures reasons = case _ of
  Result r -> Result r
  Defer x -> addFailures reasons (x unit)
  Error (PatternFailure moreReasons) ->
    Error (PatternFailure (reasons <> moreReasons))
  Error e -> Error e

evalMacro :: Ctx -> Expr -> Array Expr -> Eval Expr
evalMacro ctx0 e allArgs = case A.uncons allArgs of
  Nothing -> evalExpr ctx0 e
  Just { head: arg0, tail: args } -> Defer \_ -> do
    arg <- evalExpr ctx0 arg0
    e' <- evalFun ctx0 e
    cases <- case e' of
      Pattern p -> spy "cannot apply pattern (non-functions)" $
        throwError (NotAFunction p)
      Lambda cases -> pure cases
    let
      tryCases :: Ctx -> Array Case -> Eval Expr
      tryCases ctx = A.uncons >>> case _ of
        Nothing -> spy "absurd" $ throwError (PatternFailure empty)
        Just { head: Case (Matching patterns result) conds, tail: moreCases } ->
          case A.uncons patterns of
            -- We have a thunk, we need to force it, hope it is a lambda,
            -- and continue to evaluate its cases, and then do magic at the
            -- end to deal with `moreCases`
            -- TODO stuck
            Nothing -> fallback (\_ -> tryCases ctx moreCases) do
              ctx' <- evals ctx conds -- closurize
              evalFun ctx' result >>= case _ of
                Lambda theseCases -> Defer \_ ->
                  tryCases ctx' theseCases <#>
                    case _ of
                      Pattern final -> Pattern final
                      Lambda cases' -> Lambda $ cases' <>
                        case force (tryCases ctx moreCases) of
                          Right (Lambda cases'') -> cases''
                          Right (Pattern fallbackFn) -> [Case (Matching [] (Pattern fallbackFn)) []]
                          Left _ -> []
                Pattern p -> spy "did not force to a lambda" $
                  throwError (NotAFunction p)
            -- We have a pattern `pat` to match against `arg`:
            Just { head: pat, tail: pats } -> do
              pat' <- evalPattern ctx pat
              fallback (\_ -> tryCases ctx moreCases) do
                ctx' <- match ctx pat' arg
                case pats of
                  -- We should be able to finish evaluation here and skip
                  -- the remaining cases
                  -- TODO stuck
                  [] -> do
                    ctx'' <- evals ctx' conds
                    evalExpr ctx'' result
                  -- Awaiting more arguments, already evaluated then
                  _ -> Defer \_ -> Result $ Lambda $ A.cons
                    (Case (Matching pats result)
                      -- Add this match to the local context
                      -- (TODO: destruct it??)
                      (A.cons (Condition arg (Calling [] pat)) conds)
                    )
                    -- Back to original context!
                    (gatherCases ctx moreCases)
      gatherCases :: Ctx -> Array Case -> Array Case
      gatherCases ctx = (=<<)
        \(Case (Matching patterns result) conds) ->
          -- TODO stuck
          case A.uncons patterns of
            Nothing -> fold $ force do
              ctx' <- evals ctx conds
              evalExpr ctx' result <#> case _ of
                Lambda theseCases ->
                  gatherCases ctx' theseCases
                Pattern p -> [] -- throwError (NotAFunction p)
            Just { head: pat, tail: pats } -> fromFoldable $ force do
              pat' <- evalPattern ctx pat
              _ctx' <- match ctx pat' arg
              -- Add this match to the local context
              -- (TODO: destruct it??)
              pure $ Case (Matching pats result)
                (A.cons (Condition arg (Calling [] pat)) conds)
    e'' <- tryCases ctx0 cases
    evalMacro ctx0 e'' args

-- Evaluate macros within a pattern
evalPattern :: Ctx -> Pattern -> Eval Pattern
evalPattern ctx = case _ of
  p@(Var v) -> case force (lookupIn ctx v) of
    Left _ -> pure p
    Right (Pattern p') -> pure p'
    Right e@(Lambda _) -> spy "evalPattern Var Lambda" (throwError (NotAPattern e))
  p@(Scalar _) -> pure p
  Macro fn args ->
    evalMacro ctx fn args >>= case _ of
      Pattern p -> pure p
      e@(Lambda _) -> spy "evalPattern evalMacro Lambda" (throwError (NotAPattern e))
  Vector patterns -> Vector <$> traverse (evalPattern ctx) patterns

-- Evaluate macros within an expression
evalExpr :: Ctx -> Expr -> Eval Expr
evalExpr ctx = case _ of
  Pattern (Var v) -> evalExpr (narrow ctx v) =<< lookupIn ctx v
  p@(Pattern (Scalar _)) -> pure p
  Pattern (Macro fn args) -> evalMacro ctx fn args
  Pattern (Vector exprs) -> Pattern <<< Vector <$> traverse (evalSubExpr ctx) exprs
  Lambda cases
    | Just (Case (Matching [] result) conds) <- A.head cases ->
      -- TODO stuck
      fallback (\_ -> evalExpr ctx (Lambda (spy "evalExpr failed" (A.drop 1 cases)))) do
        ctx' <- evals ctx conds
        evalExpr ctx' result
  e@(Lambda _) -> pure e

evalSubExpr :: Ctx -> Pattern -> Eval Pattern
evalSubExpr ctx p = evalExpr ctx (Pattern p) <#> case _ of
  Lambda _ -> p
  Pattern p' -> p'

evalFun :: Ctx -> Expr -> Eval Expr
evalFun ctx = case _ of
  Pattern (Var v) -> evalFun (narrow ctx v) =<< lookupIn ctx v
  Pattern p@(Scalar _) -> spy "evalFun Scalar" (throwError (NotAFunction p))
  Pattern p@(Vector _) -> spy "evalFun Vector" (throwError (NotAFunction p))
  Pattern (Macro fn args) -> evalMacro ctx fn args
  Lambda cases
    | Just (Case (Matching [] result) conds) <- A.head cases ->
      -- TODO stuck
      fallback (\_ -> evalExpr ctx (Lambda (A.drop 1 cases))) do
        ctx' <- evals ctx conds
        evalExpr ctx' result
  e@(Lambda _) -> pure e

evals :: Ctx -> Array Condition -> Eval Ctx
evals = A.foldM \ctx (Condition fn (Calling args pat)) -> do
  result <- evalMacro ctx fn args
  match ctx pat result



match :: Ctx -> Pattern -> Expr -> Eval Ctx
match ctx = case _, _ of
  Var v, e ->
    case force (lookupIn ctx v) of
      Left _ -> pure (extend ctx v e)
      Right e' -> ctx <$ unify e e'
  Scalar s1, Pattern (Scalar s2) | s1 == s2 -> pure ctx
  Vector pats, Pattern (Vector exprs)
    | Just patexprs <- zip pats exprs ->
      A.foldM (\ctx' (Tuple pat expr) -> match ctx' pat (Pattern expr)) ctx patexprs
  Macro fn args, e ->
    evalMacro ctx fn args >>= case _ of
      Pattern p -> match ctx p e
      e'@(Lambda _) -> spy "evalMacro Lambda" (patFail e' e)
  p, e -> spy "bad match" (patFail (Pattern p) e)


unify :: Expr -> Expr -> Eval Expr
unify (Pattern p1) (Pattern p2) = Pattern <$> unifyP p1 p2
unify e1 e2 = patFail e1 e2

unifyP :: Pattern -> Pattern -> Eval Pattern
unifyP = case _, _ of
  Var v1, Var v2 | v1 == v2 -> pure (Var v1)
  Scalar s1, Scalar s2 | s1 == s2 -> pure (Scalar s1)
  Vector vs1, Vector vs2 | Just vs <- zip vs1 vs2 ->
    Vector <$> traverse (uncurry unifyP) vs
  p1, p2 -> patFail (Pattern p1) (Pattern p2)



zip :: forall a b. Array a -> Array b -> Maybe (Array (Tuple a b))
zip x y | A.length x /= A.length y = Nothing
zip x y = Just (A.zip x y)
