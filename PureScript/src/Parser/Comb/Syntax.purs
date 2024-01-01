module Parser.Comb.Syntax where

import Prelude

import Control.Plus (empty)
import Data.Array (fromFoldable, intercalate, toUnfoldable)
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Parser.Types (Part, Fragment)

data Syntax nt tok
  = Conj (Syntax nt tok) (Syntax nt tok)
  | Disj (Syntax nt tok) (Syntax nt tok)
  | Part (Part nt tok)
  | Null

derive instance eqSyntax :: (Eq nt, Eq tok) => Eq (Syntax nt tok)
derive instance ordSyntax :: (Ord nt, Ord tok) => Ord (Syntax nt tok)

printSyntax'' :: forall m nt tok. Semigroup m => (String -> m) -> Syntax nt tok -> Array (Either m (Part nt tok))
printSyntax'' t (Conj l r) =
  let
    printFactor = case _ of
      Null -> []
      -- Disj (Part p) Null -> [ Right p ] <> [ Left $ t "?" ]
      -- Disj Null (Part p) -> [ Right p ] <> [ Left $ t "?" ]
      x@(Disj _ _) -> [ Left $ t "(" ] <> printSyntax'' t x <> [ Left $ t ")" ]
      x -> printSyntax'' t x
  in join [ printFactor l, printFactor r ]
-- printSyntax'' t (Disj (Part p) Null) = [ Right p ] <> [ Left $ t "?" ]
-- printSyntax'' t (Disj Null (Part p)) = [ Right p ] <> [ Left $ t "?" ]
printSyntax'' t (Disj l r) = printSyntax'' t l <> [ Left $ t "|" ] <> printSyntax'' t r
printSyntax'' _ (Part p) = [ Right p ]
printSyntax'' _ Null = []

printSyntax' :: forall m nt tok. Monoid m => (String -> m) -> (Fragment nt tok -> m) -> Syntax nt tok -> m
printSyntax' t f syntax =
  intercalate (t " ") $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax'' t syntax

printSyntax :: forall nt tok. (Fragment nt tok -> String) -> Syntax nt tok -> String
printSyntax f syntax =
  intercalate " " $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax'' identity syntax

coalesce :: forall x m. Monoid m => Array (Either x m) -> Array (Either x m)
coalesce = toUnfoldable >>> coalesce' >>> fromFoldable

coalesce' :: forall x m. Monoid m => List (Either x m) -> List (Either x m)
coalesce' (Right x : Right y : zs) = coalesce' (Right (x <> y) : zs)
coalesce' (z : zs) = z : coalesce' zs
coalesce' Nil = Nil
