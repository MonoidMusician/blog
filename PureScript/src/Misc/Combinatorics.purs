module Misc.Combinatorics where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative, empty)
import Control.Plus (class Plus)
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Partial.Unsafe (unsafeCrashWith)

newtype Count :: Type -> Type
newtype Count a = Count Int

derive instance Functor Count
instance Apply Count where
  apply (Count a) (Count b) = Count (a * b)
instance Applicative Count where
  pure _ = Count one
instance Alt Count where
  alt (Count a) (Count b) = Count (a + b)
instance Plus Count where
  empty = Count zero
instance Alternative Count

class Alternative f <= Combinatorial f where
  choosing :: List ~> f

instance Combinatorial List where
  choosing = identity
instance Combinatorial Count where
  choosing = Count <<< List.length

chooseRange :: forall f. Combinatorial f => Int -> f Int
chooseRange x | x <= 0 = empty
chooseRange x = choosing $ List.range 0 (x - 1)

skip :: Int -> Int -> Int
skip x y
  | y < x = y
  | otherwise = y+1

-- | `fcons : fin cod -> (fin dom -> fin cod) -> (fin (dom+1) -> fin cod)`
fcons :: Int -> (Int -> Int) -> (Int -> Int)
fcons value f = \i ->
  if i == 0
    then value
    else f (i - 1)

-- | `func dom cod` enumerates functions `fin dom -> fin cod`
func :: forall f. Combinatorial f => Int -> Int -> f (Int -> Int)
func 0 cod = pure \_ -> unsafeCrashWith $ "absurd :: fin 0 -> fin " <> show cod
func dom cod = ado
  this <- chooseRange cod
  rest <- func (dom - 1) cod
  in fcons this rest

-- | `inj dom cod` enumerates injections `fin dom --> fin cod`
inj :: forall f. Combinatorial f => Int -> Int -> f (Int -> Int)
inj 0 cod = pure \_ -> unsafeCrashWith $ "absurd :: fin 0 -> fin " <> show cod
inj dom cod = ado
  this <- chooseRange cod
  rest <- inj (dom - 1) (cod - 1)
  in fcons this (skip this <<< rest)

-- | `surj dom cod` enumerates surjections `fin dom ->> fin cod`
surj :: forall f. Combinatorial f => Int -> Int -> f (Int -> Int)
surj 0 cod = pure \_ -> unsafeCrashWith $ "absurd :: fin 0 -> fin " <> show cod
surj dom cod = ado
  this <- chooseRange cod
  rest <-
    -- Either we generate a surjection that skips `this`
    (\f this -> skip this <<< f) <$> surj (dom - 1) (cod - 1)
      <|>
    -- Or we generate a surjection that includes `this`
    (\f _this -> f) <$> surj (dom - 1) cod
  in fcons this (rest this)
