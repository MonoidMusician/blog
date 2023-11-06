module Parser.Languages.TMTTMT.Eval where

import Prelude

import Control.Plus (empty)
import Data.Array as A
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Debug (spy, spyWith)
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..))

type Ctx = (String -> Maybe Expr)

lookupIn :: Ctx -> String -> Maybe Expr
lookupIn = identity

extend :: Ctx -> String -> Expr -> Ctx
extend past v e v' =
  if v == v' then Just e else past v'

fromDeclarations :: Array Declaration -> Ctx
fromDeclarations = flip Map.lookup <<< \decls ->
  let
    addDecl (DefineCase name _path casing) = SemigroupMap $
      Map.singleton name [casing]
    addDecl _ = mempty
    SemigroupMap ctx = Lambda <$> A.foldMap addDecl decls
  in ctx

evalMacro :: Ctx -> Expr -> Array Expr -> Maybe Expr
evalMacro ctx0 e allArgs = case A.uncons allArgs of
  Nothing -> evalExpr ctx0 e
  Just { head: arg0, tail: args } -> do
    arg <- evalExpr ctx0 arg0
    e' <- evalFun ctx0 e
    cases <- case e' of
      Pattern _ -> spy "cannot apply pattern (non-functions)" Nothing
      Lambda cases -> Just cases
    let
      tryCases :: Ctx -> Array Case -> Maybe Expr
      tryCases ctx = A.uncons >>> case _ of
        Nothing -> spy "absurd" Nothing
        Just { head: Case (Matching patterns result) conds, tail: moreCases } ->
          case A.uncons patterns of
            -- We have a thunk, we need to force it, hope it is a lambda,
            -- and continue to evaluate its cases, and then do magic at the
            -- end to deal with `moreCases`
            -- TODO stuck
            Nothing -> maybe' (\_ -> tryCases ctx moreCases) pure do
              ctx' <- evals ctx conds
              evalFun ctx' result >>= case _ of
                Lambda theseCases ->
                  tryCases ctx' theseCases <#>
                    case _ of
                      Pattern final -> Pattern final
                      Lambda cases' -> spy "newLambda1" $ Lambda $ cases' <>
                        case tryCases ctx moreCases of
                          Just (Lambda cases'') -> cases''
                          Just (Pattern fallback) -> [Case (Matching [] (Pattern fallback)) []]
                          Nothing -> []
                Pattern _ -> spy "did not force to a lambda" Nothing
            -- We have a pattern `pat` to match against `arg`:
            Just { head: pat, tail: pats } -> do
              pat' <- evalPattern ctx pat
              case match ctx pat' arg of
                Just ctx' ->
                  case pats of
                    -- We should be able to finish evaluation here and skip
                    -- the remaining cases
                    -- TODO stuck
                    [] ->
                      maybe' (\_ -> tryCases ctx (spy "eval failed" moreCases)) pure do
                        ctx'' <- evals ctx' conds
                        evalExpr ctx'' result
                    -- Awaiting more arguments, already evaluated then
                    _ -> Just $ spy "newLambda2" $ Lambda $ A.cons
                      (Case (Matching pats result)
                        -- Add this match to the local context
                        -- (TODO: destruct it??)
                        (A.cons (Condition arg (Calling [] pat)) conds)
                      )
                      -- Back to original context!
                      (gatherCases ctx moreCases)
                -- Failed, drop this case and continue on
                Nothing -> tryCases ctx moreCases
      gatherCases :: Ctx -> Array Case -> Array Case
      gatherCases ctx = (=<<)
        \(Case (Matching patterns result) conds) ->
          -- TODO stuck
          case A.uncons patterns of
            Nothing -> fromMaybe [] do
              ctx' <- evals ctx conds
              evalExpr ctx' result <#> case _ of
                Lambda theseCases ->
                  gatherCases ctx' theseCases
                Pattern _ -> empty
            Just { head: pat, tail: pats } -> A.fromFoldable do
              pat' <- evalPattern ctx pat
              _ctx' <- match ctx pat' arg
              -- Add this match to the local context
              -- (TODO: destruct it??)
              pure $ Case (Matching pats result)
                (A.cons (Condition arg (Calling [] pat)) conds)
    e'' <- tryCases ctx0 cases
    evalMacro ctx0 e'' args

-- Evaluate macros within a pattern
evalPattern :: Ctx -> Pattern -> Maybe Pattern
evalPattern ctx = case _ of
  p@(Var v) -> case lookupIn ctx v of
    Nothing -> Just p
    Just (Pattern p') -> Just p'
    Just (Lambda _) -> spy "evalPattern Var Lambda" Nothing
  p@(Scalar _) -> Just p
  Macro fn args ->
    evalMacro ctx fn args >>= case _ of
      Pattern p -> Just p
      Lambda _ -> spy "evalPattern evalMacro Lambda" Nothing
  Vector patterns -> Vector <$> traverse (evalPattern ctx) patterns

-- Evaluate macros within an expression
evalExpr :: Ctx -> Expr -> Maybe Expr
evalExpr ctx = case _ of
  Pattern (Var v) -> evalExpr ctx =<< lookupIn ctx v
  p@(Pattern (Scalar _)) -> Just p
  Pattern (Macro fn args) -> evalMacro ctx fn args
  Pattern pat -> Pattern <$> evalPattern ctx pat
  Lambda cases
    | Just (Case (Matching [] result) conds) <- A.head cases ->
      -- TODO stuck
      maybe' (\_ -> evalExpr ctx (Lambda (spy "evalExpr failed" (A.drop 1 cases)))) pure do
        ctx' <- evals ctx conds
        evalExpr ctx' result
  e@(Lambda _) -> Just e

evalFun :: Ctx -> Expr -> Maybe Expr
evalFun ctx = case _ of
  Pattern (Var v) -> evalFun ctx =<< lookupIn ctx v
  Pattern (Scalar _) -> spy "evalFun Scalar" Nothing
  Pattern (Macro fn args) -> evalMacro ctx fn args
  Pattern (Vector _) -> spy "evalFun Vector" Nothing
  Lambda cases
    | Just (Case (Matching [] result) conds) <- A.head cases ->
      -- TODO stuck
      maybe' (\_ -> evalExpr ctx (Lambda (A.drop 1 cases))) pure do
        ctx' <- evals ctx conds
        evalExpr ctx' result
  e@(Lambda _) -> Just e

evals :: Ctx -> Array Condition -> Maybe Ctx
evals = A.foldM \ctx (Condition fn (Calling args pat)) -> do
  result <- evalMacro ctx fn args
  match ctx pat result



match :: Ctx -> Pattern -> Expr -> Maybe Ctx
match ctx = case _, _ of
  Var v, e ->
    case lookupIn ctx v of
      Nothing -> Just (extend ctx v e)
      Just e' -> ctx <$ unify e e'
  Scalar s1, Pattern (Scalar s2) | s1 == s2 -> Just ctx
  Vector pats, Pattern (Vector exprs)
    | Just patexprs <- zip pats exprs ->
      A.foldM (\ctx' (Tuple pat expr) -> match ctx' pat (Pattern expr)) ctx patexprs
  Macro fn args, e ->
    evalMacro ctx fn args >>= case _ of
      Pattern p -> match ctx p e
      Lambda _ -> spy "evalMacro Lambda" Nothing
  p, e -> spyWith "bad match" (const { p, e }) Nothing


unify :: Expr -> Expr -> Maybe Expr
unify (Pattern p1) (Pattern p2) = Pattern <$> unifyP p1 p2
unify _ _ = Nothing

unifyP :: Pattern -> Pattern -> Maybe Pattern
unifyP = case _, _ of
  Var v1, Var v2 | v1 == v2 -> Just (Var v1)
  Scalar s1, Scalar s2 | s1 == s2 -> Just (Scalar s1)
  Vector vs1, Vector vs2 | Just vs <- zip vs1 vs2 ->
    Vector <$> traverse (uncurry unifyP) vs
  _, _ -> Nothing



zip :: forall a b. Array a -> Array b -> Maybe (Array (Tuple a b))
zip x y | A.length x /= A.length y = Nothing
zip x y = Just (A.zip x y)
