module Parser.Comb.Syntax where

import Prelude

import Data.Array (fromFoldable, intercalate, toUnfoldable)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Parser.Types (Part, Fragment)

data Syntax meta nt tok
  = Conj (Syntax meta nt tok) (Syntax meta nt tok)
  | Disj (Syntax meta nt tok) (Syntax meta nt tok)
  | Part (Part meta nt tok)
  | Null

derive instance eqSyntax :: (Eq meta, Eq nt, Eq tok) => Eq (Syntax meta nt tok)
derive instance ordSyntax :: (Ord meta, Ord nt, Ord tok) => Ord (Syntax meta nt tok)

printSyntax'' :: forall m meta nt tok. Semigroup m => (String -> m) -> Syntax meta nt tok -> Array (Either m (Part meta nt tok))
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

printSyntax' :: forall m meta nt tok. Monoid m => (String -> m) -> (Fragment meta nt tok -> m) -> Syntax meta nt tok -> m
printSyntax' t f syntax =
  intercalate (t " ") $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax'' t syntax

printSyntax :: forall meta nt tok. (Fragment meta nt tok -> String) -> Syntax meta nt tok -> String
printSyntax f syntax =
  intercalate " " $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax'' identity syntax

coalesce :: forall x m. Monoid m => Array (Either x m) -> Array (Either x m)
coalesce = toUnfoldable >>> coalesce' >>> fromFoldable

coalesce' :: forall x m. Monoid m => List (Either x m) -> List (Either x m)
coalesce' (Right x : Right y : zs) = coalesce' (Right (x <> y) : zs)
coalesce' (z : zs) = z : coalesce' zs
coalesce' Nil = Nil
