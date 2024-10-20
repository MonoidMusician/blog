module Parser.Comb.Syntax where

import Prelude

import Data.Array (fromFoldable, intercalate, toUnfoldable)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Function (applyN)
import Data.List (List(..), (:))
import Parser.Types (Part, Fragment)

data Syntaxed space nt tok
  = Paren (Syntaxed space nt tok)
  | Opt (Syntaxed space nt tok)
  | Conjs (Array (Syntaxed space nt tok))
  | Disjs (Array (Syntaxed space nt tok))
  | Atom (Part space nt tok)

syntaxed :: forall space nt tok. Syntax space nt tok -> Syntaxed space nt tok
syntaxed = case _ of
  Null -> Paren (Conjs [])
  Part p -> Atom p
  c@(Conj _ _) -> Conjs $ collectConjs c <#> case _ of
    s@(Disjs _) -> Paren s
    s -> s
  d@(Disj _ _) ->
    case Array.partition isNull $ collectDisjs d of
      { no: [], yes: [] } -> Disjs [] -- should not happen
      { no: [no], yes: [] } -> no -- also should not happen
      { no: [], yes } -> applyN Opt (Array.length yes - 1) (Paren (Conjs []))
      { no, yes: [] } -> Disjs no
      { no: [no], yes } -> applyN Opt (Array.length yes) no
      { no, yes } -> applyN Opt (Array.length yes) (Paren (Disjs no))
  where
  isNull (Conjs []) = true
  isNull _ = false
  collectConjs (Conj l r) = collectConjs l <> collectConjs r
  collectConjs o = [syntaxed o]
  collectDisjs (Disj l r) = collectDisjs l <> collectDisjs r
  collectDisjs o = [syntaxed o]

data Syntax space nt tok
  = Conj (Syntax space nt tok) (Syntax space nt tok)
  | Disj (Syntax space nt tok) (Syntax space nt tok)
  | Part (Part space nt tok)
  | Null

derive instance eqSyntax :: (Eq space, Eq nt, Eq tok) => Eq (Syntax space nt tok)
derive instance ordSyntax :: (Ord space, Ord nt, Ord tok) => Ord (Syntax space nt tok)

printSyntax'' :: forall m space nt tok. Semigroup m => (String -> m) -> Syntax space nt tok -> Array (Either m (Part space nt tok))
printSyntax'' t (Conj l r) =
  let
    printFactor = case _ of
      Null -> []
      Disj (Part p) Null -> [ Right p ] <> [ Left $ t "?" ]
      Disj Null (Part p) -> [ Right p ] <> [ Left $ t "?" ]
      Disj x Null -> [ Left $ t "(" ] <> printSyntax'' t x <> [ Left $ t ")?" ]
      Disj Null x -> [ Left $ t "(" ] <> printSyntax'' t x <> [ Left $ t ")?" ]
      x@(Disj _ _) -> [ Left $ t "(" ] <> printSyntax'' t x <> [ Left $ t ")" ]
      x -> printSyntax'' t x
  in join [ printFactor l, printFactor r ]
printSyntax'' t (Disj (Part p) Null) = [ Right p ] <> [ Left $ t "?" ]
printSyntax'' t (Disj Null (Part p)) = [ Right p ] <> [ Left $ t "?" ]
printSyntax'' t (Disj l r) = printSyntax'' t l <> [ Left $ t "|" ] <> printSyntax'' t r
printSyntax'' _ (Part p) = [ Right p ]
printSyntax'' _ Null = []

printSyntax' :: forall m space nt tok. Monoid m => (String -> m) -> (Fragment space nt tok -> m) -> Syntax space nt tok -> m
printSyntax' t f syntax =
  intercalate (t " ") $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax'' t syntax

printSyntax :: forall space nt tok. (Fragment space nt tok -> String) -> Syntax space nt tok -> String
printSyntax f syntax =
  intercalate " " $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax'' identity syntax

coalesce :: forall x m. Monoid m => Array (Either x m) -> Array (Either x m)
coalesce = toUnfoldable >>> coalesce' >>> fromFoldable

coalesce' :: forall x m. Monoid m => List (Either x m) -> List (Either x m)
coalesce' (Right x : Right y : zs) = coalesce' (Right (x <> y) : zs)
coalesce' (z : zs) = z : coalesce' zs
coalesce' Nil = Nil
