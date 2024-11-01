{-# LANGUAGE MagicHash, TypeFamilies, OverloadedLists, DataKinds, RoleAnnotations, UnboxedTuples, PatternSynonyms, ViewPatterns #-}
module Levity where

-- Just silly experiments with levity polymorphism
--
-- Trying to see if we can have a unified array type that automatically does
-- the right thing. Unfortunately you cannot really build abstract APIs on top
-- of it, since variables have to have a fixed runtime representation. And
-- `Show#` does not slot into the rest of the ecosystem well at all ...

-- anyways, uh, only implemented for `Int#` but you could imagine it expanded
-- for all the integer types, and possibly even tuples, yeah that would be
-- fun but more work

import GHC.Base
import GHC.Arr (Array)
import Unsafe.Coerce (unsafeCoerce)
import qualified GHC.Arr as Array
import qualified Data.List as List
import qualified Data.Array.IArray as IArray
import GHC.ST (runST, ST (ST))
import Data.Proxy (Proxy (Proxy))

instance Show t => Show (Awray t) where
  show awr = show $ (\(I# i#) -> awr ! i#) <$> indices awr

instance Show (Awray Int#) where
  show awr = show $ (\(I# i#) -> shown# (awr ! i#)) <$> indices awr

newtype Shown = Shown String
instance Show Shown where show (Shown shown) = shown

class Show# (t :: TYPE r) where
  show# :: t -> String
  shown# :: t -> Shown
instance Show# Int# where
  show# i# = show (I# i#) <> "#"
  shown# i# = Shown (show# i#)

class Showy (r :: RuntimeRep) where
  type ShowIt :: TYPE r -> Constraint
  showy :: forall (t :: TYPE r). ShowIt t => t -> String

instance Showy LiftedRep where
  type ShowIt = Show
  showy = show
-- proper instance chains would be nice
-- (no i don't trust OVERLAPPING/OVERLAPPABLE)
instance Showy IntRep where
  type ShowIt = Show#
  showy = show#


-- is there a better way to have a data type with no fixed repr?
data Awray :: forall r. TYPE r -> Type
pattern Awray :: AwrayRep t -> Awray t
pattern Awray arr <- (fromAwray -> arr) where
  Awray arr = toAwray arr

-- reimplementing `UArray Int`, basically, but for unboxèds
type role TypedArray nominal
data TypedArray (t :: TYPE r) = TypedArray Int# ByteArray#

{-# COMPLETE TypedAwray #-} -- why is this necessary? lol
pattern TypedAwray :: forall r (t :: TYPE r). (AwrayOf r, AwrayRep t ~ TypedArray t) => Int# -> ByteArray# -> Awray t
pattern TypedAwray l arr <- (fromAwray -> TypedArray l arr) where
  TypedAwray l arr = toAwray (TypedArray l arr)


-- basically every abstract operation you would want has to actually live in
-- the class, otherwise it will complain about lack of fixed runtime rep.
class AwrayOf r where
  type AwrayRep :: TYPE r -> Type
  type Aww :: TYPE r -> Constraint
  toAwray :: forall (t :: TYPE r). AwrayRep t -> Awray t
  fromAwray :: forall (t :: TYPE r). Awray t -> AwrayRep t
  singleton :: forall (t :: TYPE r). Aww t => t -> Awray t
  fromList :: forall i (t :: TYPE r). Aww t => [i] -> (i -> t) -> Awray t
  idx :: forall (t :: TYPE r) c. Aww t => Int# -> Awray t -> c -> (t -> c) -> c
  (!) :: forall (t :: TYPE r). Aww t => Awray t -> Int# -> t
  length# :: forall (t :: TYPE r). Awray t -> Int#

indices :: forall r (t :: TYPE r). AwrayOf r => Awray t -> [Int]
indices awr = enumFromTo 0 (I# (length# awr -# 1#))

-- trivial constraint
class Always t
instance Always t

-- arrays of normal (boxed, lazy) types
instance AwrayOf LiftedRep where
  type AwrayRep = Array Int
  type Aww = Always
  toAwray = unsafeCoerce
  fromAwray = unsafeCoerce
  singleton v = toAwray (Array.listArray (0, 0) [v])
  fromList vs f = toAwray (Array.listArray (0, List.length vs - 1) (f <$> vs))
  idx i# awr m f = case fromAwray awr of
    arr | IArray.inRange (IArray.bounds arr) (I# i#) ->
      f (arr IArray.! I# i#)
    _ -> m
  Awray arr ! i# = arr IArray.! I# i#
  length# (Awray arr) =
    let !(I# i) = IArray.rangeSize (IArray.bounds arr) in i

-- arrays of `Int#`
instance AwrayOf IntRep where
  type AwrayRep = TypedArray
  -- assume that `Int#` is the only `TYPE IntRep` *that we care about
  type Aww = (~) Int#
  toAwray = unsafeCoerce
  fromAwray = unsafeCoerce
  singleton v = runST $ ST \s1 ->
    let
      !(# s2#, arr# #) = newByteArray# (width# (Proxy @IntRep)) s1
      s3# = writeIntArray# arr# 0# v s2#
      !(# s4#, awr# #) = unsafeFreezeByteArray# arr# s3#
    in (# s4#, TypedAwray 1# awr# #)
  fromList vs0 f = runST $ ST \s1 ->
    let
      !(I# l#) = List.length vs0
      !(# s2#, arr# #) = newByteArray# (l# *# width# (Proxy @IntRep)) s1
      go i vs s# = case vs of
        [] -> s#
        (v : vs') ->
          let
            s'# = writeIntArray# arr# i (f v) s#
          in go (i +# 1#) vs' s'#
      s3# = go 0# vs0 s2#
      !(# s4#, awr# #) = unsafeFreezeByteArray# arr# s3#
    in (# s4#, TypedAwray l# awr# #)
  idx i# awr m f = case fromAwray awr of
    TypedArray l# arr# | isTrue# (i# <# l#) -> f (indexIntArray# arr# i#)
    _ -> m
  TypedAwray _ arr# ! i# = indexIntArray# arr# i#
  length# (TypedAwray l# _) = l#

main :: IO ()
main = do
  print (singleton 157842 :: Awray Int)
  print (singleton 157842# :: Awray Int#)
  print (fromList [157,842] id :: Awray Int)
  print (fromList [157,842] (\(I# i#) -> i#) :: Awray Int#)
  pure ()

-- is there a better way to do this? 64-bit...
class Width (r :: RuntimeRep) where
  width# :: Proxy r -> Int#
instance Width IntRep where width# _ = 8#


{-
class Unboxed (t :: TYPE r) where
  type Box t :: Type
  fromBox :: Box t -> t
  toBox :: t -> Box t

instance Unboxed Int# where
  type Box Int# = Int
  fromBox (I# i#) = i#
  toBox = I#

-- The pattern synonym argument does not have a fixed runtime representation:
-- • t :: TYPE r
pattern Box :: forall r (t :: TYPE r). Unboxed t => t -> Box t
pattern Box dat# <- (fromBox -> dat#) where
  Box dat# = toBox dat#
-}


