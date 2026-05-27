module Control.Monad.Cell.Interface where

import Prelude

import Data.Const (Const(..))
import Data.Exists (Exists, mkExists, runExists)

type Interface :: (Type -> Type) -> Type -> Type -> Type
type Interface m i o = i -> m o


-- | A keyed interface, where input and output.
newtype KInterface :: forall k. (Type -> Type) -> (k -> Type) -> (k -> Type) -> Type
newtype KInterface m i o = KInterface (forall @k. i k -> m (o k))


toSimple :: forall m i o. Functor m => KInterface m (Const i) (Const o) -> Interface m i o
toSimple (KInterface f) i = (\(Const o) -> o) <$> f (Const i)

fromSimple :: forall m i o. Functor m => Interface m i o -> KInterface m (Const i) (Const o)
fromSimple f = KInterface \(Const i) -> Const <$> f i


-- | A pair of an input and output type, existentially keyed together.
newtype KIOPair :: forall k. (Type -> Type) -> (k -> Type) -> (k -> Type) -> Type
newtype KIOPair m input output = KIOPair (Exists (KIOPairF m input output))

data KIOPairF :: forall k. (Type -> Type) -> (k -> Type) -> (k -> Type) -> k -> Type
data KIOPairF m input output key = KIOPairF (input key) (output key -> m Unit)

mkKIOPair :: forall m i o k. i k -> (o k -> m Unit) -> KIOPair m i o
mkKIOPair input output = KIOPair (mkExists (KIOPairF input output))

unKIOPair :: forall m i o z. KIOPair m i o -> (forall k. i k -> (o k -> m Unit) -> z) -> z
unKIOPair (KIOPair e) f = runExists (\(KIOPairF i o) -> f i o) e

runKIOPair :: forall m i o z. (forall k. i k -> (o k -> m Unit) -> z) -> KIOPair m i o -> z
runKIOPair f (KIOPair e) = runExists (\(KIOPairF i o) -> f i o) e
