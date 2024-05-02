module Parser.Printer.Prec where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Array.NonEmpty as NEA
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

genPrec :: Gen (Prec Boolean)
genPrec = Gen.oneOf $ NEA.cons' (pure NoPrec)
  [ pure AnyPrec
  , pure DiffPrec
  , Prec <$> Gen.enum
  , MTPrec <$> Gen.enum
  ]

main :: Effect Unit
main = checkSemiringGen genPrec

checkSemiringGen
  ∷ ∀ a
  . Semiring a
  ⇒ Eq a
  ⇒ Gen a
  → Effect Unit
checkSemiringGen gen = do
  let amt = 100000
  log "Checking 'Associativity' law for Semiring addition"
  quickCheck' amt $ lift3 associativeAddition gen gen gen

  log "Checking 'Identity' law for Semiring addition"
  quickCheck' amt $ identityAddition <$> gen

  log "Checking 'Commutative' law for Semiring addition"
  quickCheck' amt $ lift2 commutativeAddition gen gen

  log "Checking 'Associativity' law for Semiring multiplication"
  quickCheck' amt $ lift3 associativeMultiplication gen gen gen

  log "Checking 'Identity' law for Semiring multiplication"
  quickCheck' amt $ identityMultiplication <$> gen

  log "Checking 'Left distribution' law for Semiring"
  quickCheck' amt $ lift3 leftDistribution gen gen gen

  log "Checking 'Right distribution' law for Semiring"
  quickCheck' amt $ lift3 rightDistribution gen gen gen

  log "Checking 'Annihilation' law for Semiring"
  quickCheck' amt $ annihiliation <$> gen

  where

  associativeAddition ∷ a → a → a → Boolean
  associativeAddition a b c = (a + b) + c == a + (b + c)

  identityAddition ∷ a → Boolean
  identityAddition a = (zero + a == a) && (a + zero == a)

  commutativeAddition ∷ a → a → Boolean
  commutativeAddition a b = a + b == b + a

  associativeMultiplication ∷ a → a → a → Boolean
  associativeMultiplication a b c = (a * b) * c == a * (b * c)

  identityMultiplication ∷ a → Boolean
  identityMultiplication a = (one * a == a) && (a * one == a)

  leftDistribution ∷ a → a → a → Boolean
  leftDistribution a b c = a * (b + c) == (a * b) + (a * c)

  rightDistribution ∷ a → a → a → Boolean
  rightDistribution a b c = (a + b) * c == (a * c) + (b * c)

  annihiliation ∷ a → Boolean
  annihiliation a = (a * zero == zero) && (zero * a == zero)

data Prec v
  = NoPrec
  | AnyPrec
  | DiffPrec
  | Prec v
  | MTPrec v

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
