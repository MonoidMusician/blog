module Parser.Languages.TMTTMT.TypeCheck where

import Prelude

import Data.Array as A
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Expr(..), Matching(..), Pattern)

-- Monoid x ???
class TypeSystem ty x
  | ty -> x, x -> ty
  where
  unFunAbs :: ty -> (ty -> ty -> x) -> x
  unFunApp :: Expr -> Array (ty -> x) -> (ty -> x) -> x
  pattern :: Pattern -> ty -> x -> (ty -> x) -> x
  construct :: Pattern -> ty -> x
  failureCase :: ty -> x

class TypeSystem ty x <= FullTypeSystem ty x
  | ty -> x, x -> ty
  where
  unions :: Array ty -> ty
  mkFun :: Array ty -> ty -> ty
  lookup :: String -> (ty -> x) -> x
  coercion :: x -> ty -> ty -> x
  synExpr :: Expr -> (ty -> x -> x) -> x

typecheckCases :: forall ty x. TypeSystem ty x => Array Case -> ty -> x
typecheckCases cases ty =
  A.foldr (flip typecheckCase ty) (failureCase ty) cases

constructExpr :: forall ty x. TypeSystem ty x => Expr -> ty -> x
constructExpr (Lambda cases) = typecheckCases cases
constructExpr (Pattern pat) = construct pat

typecheckCase :: forall ty x. TypeSystem ty x => Case -> ty -> x -> x
typecheckCase (Case matching conditions) ty fallback =
  typecheckMatching matching ty fallback $
    A.foldr (\cond yes -> condition cond yes fallback) <@> conditions

typecheckMatching :: forall ty x. TypeSystem ty x => Matching -> ty -> x -> (x -> x) -> x
typecheckMatching (Matching patterns result) ty0 fallback body =
  A.foldr
    (\pat yes ty -> unFunAbs ty \i o -> pattern pat i (yes o) (const fallback))
    (\ty -> body (constructExpr result ty))
    patterns
    ty0

condition :: forall ty x. TypeSystem ty x => Condition -> x -> x -> x
condition (Condition fn (Calling args pat)) yes no =
  unFunApp fn (constructExpr <$> args) \ty -> pattern pat ty yes (const no)
