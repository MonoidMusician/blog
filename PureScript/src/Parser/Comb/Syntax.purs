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

printSyntax' :: forall m nt tok. Semigroup m => (String -> m) -> Syntax nt tok -> Array (Either m (Part nt tok))
printSyntax' t (Conj l r) =
  let
    p x = case x of
      Null -> empty
      Disj _ _ -> [ Left $ t "(" ] <> printSyntax' t x <> [ Left $ t ")" ]
      _ -> printSyntax' t x
  in join [ p l, p r ]
printSyntax' t (Disj l r) = printSyntax' t l <> [ Left $ t " | " ] <> printSyntax' t r
printSyntax' _ (Part p) = [ Right p ]
printSyntax' _ Null = []

printSyntax :: forall nt tok. (Fragment nt tok -> String) -> Syntax nt tok -> String
printSyntax f syntax =
  intercalate " " $ map (either identity f) $ coalesce $ map (map pure) $ printSyntax' identity syntax

coalesce :: forall x m. Monoid m => Array (Either x m) -> Array (Either x m)
coalesce = toUnfoldable >>> coalesce' >>> fromFoldable

coalesce' :: forall x m. Monoid m => List (Either x m) -> List (Either x m)
coalesce' (Right x : Right y : zs) = coalesce' (Right (x <> y) : zs)
coalesce' (z : zs) = z : coalesce' zs
coalesce' Nil = Nil
