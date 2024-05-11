module Parser.Printer.Prec where

import Prelude

import Data.Maybe (Maybe(..))

data Prec v
  = NoPrec
  | AnyPrec
  | DiffPrec
  | Prec v
  | MTPrec v

unPrec :: forall v. Prec v -> Maybe v
unPrec (Prec v) = Just v
unPrec _ = Nothing -- yes, MTPrec does not count

derive instance eqPrec :: Eq v => Eq (Prec v)

instance semiringPrec :: Eq v => Semiring (Prec v) where
  zero = AnyPrec
  one = NoPrec

  add DiffPrec _ = DiffPrec
  add _ DiffPrec = DiffPrec
  add AnyPrec v = v
  add v AnyPrec = v
  add NoPrec NoPrec = NoPrec
  add NoPrec (Prec v) = MTPrec v
  add (Prec v) NoPrec = MTPrec v
  add NoPrec (MTPrec v) = MTPrec v
  add (MTPrec v) NoPrec = MTPrec v
  add (Prec v1) (Prec v2)
    | v1 == v2 = Prec v1
    | otherwise = DiffPrec
  add (MTPrec v1) (MTPrec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec
  add (Prec v1) (MTPrec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec
  add (MTPrec v1) (Prec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec

  mul AnyPrec _ = AnyPrec
  mul _ AnyPrec = AnyPrec
  mul NoPrec v = v
  mul v NoPrec = v
  mul _ DiffPrec = DiffPrec
  mul DiffPrec (MTPrec _) = DiffPrec
  mul (Prec v1) (MTPrec v2)
    | v1 == v2 = Prec v2
    | otherwise = DiffPrec
  mul (MTPrec v1) (MTPrec v2)
    | v1 == v2 = MTPrec v1
    | otherwise = DiffPrec
  mul _ (Prec v) = Prec v
