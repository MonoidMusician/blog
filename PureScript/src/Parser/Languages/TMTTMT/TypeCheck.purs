module Parser.Languages.TMTTMT.TypeCheck where

import Prelude

import Data.Array as A
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Expr, Matching(..), Pattern)

class Monoid x <= TypeSystem ty x
  | ty -> x, x -> ty
  where
  -- unions :: Array ty -> ty
  -- mkFun :: Array ty -> ty -> ty
  unFunAbs :: ty -> (ty -> ty -> x) -> x
  unFunApp :: Expr -> Array (ty -> x) -> (ty -> x) -> x
  -- lookup :: String -> (ty -> x) -> x
  pattern :: Pattern -> ty -> x -> x -> x
  construct :: Expr -> ty -> x
  -- coercion :: ty -> ty -> x
  -- synExpr :: Expr -> (ty -> x -> x) -> x
  fail :: ty -> x

typecheckCases :: forall ty x. TypeSystem ty x => Array Case -> ty -> x
typecheckCases cases ty =
  A.foldr (flip typecheckCase ty) (fail ty) cases

typecheckCase :: forall ty x. TypeSystem ty x => Case -> ty -> x -> x
typecheckCase (Case matching conditions) ty fallback =
  typecheckMatching matching ty fallback $
    A.foldr (\cond yes -> condition cond yes fallback) <@> conditions

typecheckMatching :: forall ty x. TypeSystem ty x => Matching -> ty -> x -> (x -> x) -> x
typecheckMatching (Matching patterns result) ty0 fallback body =
  A.foldr
    (\pat yes ty -> unFunAbs ty \i o -> pattern pat i (yes o) fallback)
    (\ty -> body (construct result ty))
    patterns
    ty0

condition :: forall ty x. TypeSystem ty x => Condition -> x -> x -> x
condition (Condition fn (Calling args pat)) yes no =
  unFunApp fn (construct <$> args) \ty -> pattern pat ty yes no
