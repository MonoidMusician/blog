{-# LANGUAGE MagicHash, UnboxedTuples #-}
{- HLINT ignore "Use tuple-section" -}
module UserData where

import Prelude
import Data.Typeable (Typeable, TypeRep, typeRep, Proxy(..), cast)
import Data.Hashable (Hashable(..))
import GHC.StableName (StableName (StableName), makeStableName, eqStableName)
import GHC.IO (unsafePerformIO, IO (IO))
import Control.Concurrent.STM (TVar)
import GHC.Weak (Weak (Weak), mkWeak)
import GHC.Base (mkWeak#, makeStableName#, StableName#, TVar#, RealWorld, MVar#, MutVar#)
import GHC.Conc.Sync (TVar(TVar))
import GHC.MVar (MVar(MVar))
import GHC.IORef (IORef(IORef))
import GHC.STRef (STRef(STRef))
import GHC.Exts (unsafeCoerce#)
import Data.Foldable (traverse_)
import qualified Data.HashTable.IO as H

-- Hash table implementation
type HashTable k v = H.BasicHashTable k v

-- | A (global or scoped) table containing pointers (as stable names) and user data.
type UserDataTable = HashTable SomeStableName UserDataStore

-- | This is a store for the user data associated with a particular pointer. It
-- separates the user data by its TypeRep.
type UserDataStore = HashTable TypeRep SomeUserData

-- | One common store for all global user data.
{-# NOINLINE globalUserData #-}
globalUserData :: UserDataTable
globalUserData = unsafePerformIO H.new

-- | Make a scoped user data store.
scopedUserData :: IO UserDataTable
scopedUserData = H.new

data SomeUserData = forall d. IsUserData d => SomeUserData d

data SomeStableName = forall t. SomeStableName (StableName t)
instance Hashable SomeStableName where
  hash (SomeStableName x) = hash x
  hashWithSalt sel (SomeStableName x) = hashWithSalt sel x
instance Eq SomeStableName where
  SomeStableName x == SomeStableName y = eqStableName x y

class Typeable d => IsUserData d where
  discardUD :: d -> IO ()

discardAllUD :: UserDataStore -> IO ()
discardAllUD = H.mapM_ \(_, SomeUserData d) -> discardUD d

deleteAllUD :: UserDataTable -> SomeStableName -> IO ()
deleteAllUD table k = do
  removed <- H.mutate table k \extant -> (Nothing, extant)
  traverse_ discardAllUD removed

-- | A class for GC data with some useful form of referential identity such that
-- StableName and Weak ref finalizers work for it.
class HasUserData t where
  userDataAt :: t -> IO (StableName t, IO () -> IO (Weak t))
  -- Force the thunk, then make a stable name and finalizer adder
  userDataAt !t = do
    name <- makeStableName t
    pure (name, mkWeak t t . Just)

modifyUDOn :: forall t d r. HasUserData t => IsUserData d => t -> (Maybe d -> IO (r, Maybe d)) -> IO r
modifyUDOn = modifyUDOnTable globalUserData

modifyUDOnTable :: forall t d r. HasUserData t => IsUserData d => UserDataTable -> t -> (Maybe d -> IO (r, Maybe d)) -> IO r
modifyUDOnTable table t f = do
  (!tname, track) <- userDataAt t
  let (!name, !ty) = (SomeStableName tname, typeRep (Proxy @d))
  prevUD <- H.lookup table name
  prevD <- case prevUD of
    Just store -> do
      found <- H.lookup store ty
      case found of
        Just (SomeUserData s) -> pure $ cast s
        Nothing -> pure Nothing
    Nothing -> pure Nothing
  (ret, next) <- f prevD
  ret <$ case (prevUD, next) of
    (Nothing, Nothing) -> pure ()
    (Just prev, Just nextD) -> do
      H.insert prev ty (SomeUserData nextD)
    (Nothing, Just nextD) -> do
      (_ :: Weak t) <- track (deleteAllUD table name)
      prev <- H.new
      H.insert prev ty (SomeUserData nextD)
      H.insert table name prev
    (Just prev, Nothing) -> do
      H.delete prev ty
      -- TODO: delete from table?

-- Mutable cells are one construct that have distinct referential identity, so
-- they should be good candidates for stable names. Ironically, because of the
-- distinction between lifted and unlifted wrappers, they require special
-- handling to tag the underlying primitive pointers, not the lifted wrappers
-- which both may be unevaluated thunks and freely reconstructed around the
-- resource.
--
-- Other data structures (like expression ASTs) may also be good candidates for
-- stably named user data.

-- | Tag the underlying TVar# primitive pointer, not the TVar lifted wrapper which may be optimized away/recreated by GHC
instance HasUserData (TVar t) where
  userDataAt var@(TVar var#) = IO \s0# ->
    let
      cont (IO destr) = IO \s2# ->
        case mkWeak# var# var destr s2# of
          (# s3, w #) -> (# s3, Weak w #)
      coe = unsafeCoerce# :: StableName# (TVar# RealWorld t) -> StableName# (TVar t)
    in case makeStableName# var# s0# of
      (# s1#, name# #) -> (# s1#, (StableName (coe name#), cont) #)
-- | Tag the underlying MVar# primitive pointer, not the MVar lifted wrapper which may be optimized away/recreated by GHC
instance HasUserData (MVar t) where
  userDataAt var@(MVar var#) = IO \s0# ->
    let
      cont (IO destr) = IO \s2# ->
        case mkWeak# var# var destr s2# of
          (# s3, w #) -> (# s3, Weak w #)
      coe = unsafeCoerce# :: StableName# (MVar# RealWorld t) -> StableName# (MVar t)
    in case makeStableName# var# s0# of
      (# s1#, name# #) -> (# s1#, (StableName (coe name#), cont) #)
-- | Tag the underlying MutVar# primitive pointer, not the STRef/IORef lifted wrapper which may be optimized away/recreated by GHC
instance HasUserData (IORef t) where
  userDataAt var@(IORef (STRef var#)) = IO \s0# ->
    let
      cont (IO destr) = IO \s2# ->
        case mkWeak# var# var destr s2# of
          (# s3, w #) -> (# s3, Weak w #)
      coe = unsafeCoerce# :: StableName# (MutVar# RealWorld t) -> StableName# (IORef t)
    in case makeStableName# var# s0# of
      (# s1#, name# #) -> (# s1#, (StableName (coe name#), cont) #)
-- | Tag the underlying MutVar# primitive pointer, not the STRef lifted wrapper which may be optimized away/recreated by GHC
instance HasUserData (STRef s t) where
  userDataAt var@(STRef var#) = IO \s0# ->
    let
      cont (IO destr) = IO \s2# ->
        case mkWeak# var# var destr s2# of
          (# s3, w #) -> (# s3, Weak w #)
      coe = unsafeCoerce# :: StableName# (MutVar# s t) -> StableName# (STRef s t)
    in case makeStableName# var# s0# of
      (# s1#, name# #) -> (# s1#, (StableName (coe name#), cont) #)


