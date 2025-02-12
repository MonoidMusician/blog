module Web.Audio.FFI where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import _unsafeGetProperty :: forall obj val. obj -> String -> Effect val
foreign import _unsafeSetProperty :: forall obj val. obj -> String -> val -> Effect Unit
foreign import _undefined :: forall a. a

class FFI t d | t -> d where
  toFFI :: t -> d
  fromFFI :: d -> t

instance FFI Number Number where
  toFFI = identity
  fromFFI = identity

instance FFI Int Int where
  toFFI = identity
  fromFFI = identity

instance FFI String String where
  toFFI = identity
  fromFFI = identity

instance FFI Boolean Boolean where
  toFFI = identity
  fromFFI = identity

instance (RL.RowToList tr tl, FFIRL tr tl dl dr) => FFI (Record tr) (Record dr) where
  toFFI = toFFIRecord (Proxy :: Proxy tl)
  fromFFI = fromFFIRecord (Proxy :: Proxy tl)

class (RL.ListToRow tl tr, RL.ListToRow dl dr) <= FFIRL tr tl dl dr | tl -> tr dl, dl -> dr where
  toFFIRecord :: Proxy tl -> Record tr -> Record dr
  fromFFIRecord :: Proxy tl -> Record dr -> Record tr

instance FFIRL () RL.Nil RL.Nil () where
  toFFIRecord _ _ = {}
  fromFFIRecord _ _ = {}

foreign import data OrUndefined :: Type -> Type

instance FFI t d => FFI (Maybe t) (OrUndefined d) where
  toFFI = maybe _undefined (defined <<< toFFI)
  fromFFI = isDefined >>> map fromFFI

defined :: forall a. a -> OrUndefined a
defined = unsafeCoerce

isDefined :: forall a. OrUndefined a -> Maybe a
isDefined a | unsafeRefEq a _undefined = Nothing
isDefined a = Just (unsafeCoerce a)

instance
  ( FFI t d
  , FFIRL tr tl dl dr
  , Row.Cons s (Maybe t) tr tr'
  , Row.Cons s (OrUndefined d) dr dr'
  , IsSymbol s
  , Row.Lacks s tr
  , Row.Lacks s dr
  ) => FFIRL tr' (RL.Cons s (Maybe t) tl) (RL.Cons s (OrUndefined d) dl) dr' where
    toFFIRecord _ r =
      let
        s = Proxy :: Proxy s
        mt = Record.get s r :: Maybe t
        r' = toFFIRecord (Proxy :: Proxy tl) (unsafeCoerce r :: Record tr)
      in case mt of
        Nothing -> unsafeCoerce r' :: Record dr'
        Just t -> Record.insert s (defined (toFFI t)) r'
    fromFFIRecord _ r =
      let
        s = Proxy :: Proxy s
        md = Record.get s r :: OrUndefined d
        r' = fromFFIRecord (Proxy :: Proxy tl) (unsafeCoerce r :: Record dr)
      in Record.insert s (fromFFI <$> isDefined md) r'

else instance
  ( FFI t d
  , FFIRL tr tl dl dr
  , Row.Cons s t tr tr'
  , Row.Cons s d dr dr'
  , IsSymbol s
  , Row.Lacks s tr
  , Row.Lacks s dr
  ) => FFIRL tr' (RL.Cons s t tl) (RL.Cons s d dl) dr' where
    toFFIRecord _ r =
      let
        s = Proxy :: Proxy s
        t = Record.get s r :: t
      in Record.insert s (toFFI t)
        (toFFIRecord (Proxy :: Proxy tl) (unsafeCoerce r :: Record tr))
    fromFFIRecord _ r =
      let
        s = Proxy :: Proxy s
        d = Record.get s r :: d
      in Record.insert s (fromFFI d)
        (fromFFIRecord (Proxy :: Proxy tl) (unsafeCoerce r :: Record dr))
