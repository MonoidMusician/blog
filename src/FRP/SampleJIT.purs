module FRP.SampleJIT where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Foldable (traverse_)
import Data.Newtype (unwrap)
import Deku.Core (class Korok)
import Effect.AVar as AVarE
import Effect.Aff.AVar as AVar
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Event (AnEvent, Event, fromEvent, makeEvent, subscribe, toEvent)
import Type.Equality (class TypeEquals, to)

-- | Samples the first event JIT when the second event is actually used.
-- | This is like sampleOn, but it defers the sampling until the ref is consumed.
-- | Useful when the second event is an effectful callback
-- | in which case we do not want to recreate the callback
-- | every time the first event fires.
sampleJIT :: forall a b. Event a -> Event (AVar.AVar a -> b) -> Event b
sampleJIT e ae = makeEvent \k -> do
  av <- AVarE.empty
  u0 <- subscribe e \v -> do
    void $ AVarE.tryTake av *> AVarE.tryPut v av
  u1 <- subscribe ae \v -> k (v av)
  pure (u0 *> u1)

withSample :: forall n a. MonadEffect n => (a -> n Unit) -> ReaderT (AVar.AVar a) n Unit
withSample cont = ReaderT \var ->
  liftEffect (AVarE.tryRead var) >>= traverse_ cont

withSample2 :: forall n a. MonadEffect n => ReaderT a n Unit -> (AVar.AVar a) -> n Unit
withSample2 (ReaderT cont) = \var ->
  liftEffect (AVarE.tryRead var) >>= traverse_ cont

withSampleE :: forall n a. MonadEffect n => Event (a -> n Unit) -> Event (ReaderT (AVar.AVar a) n Unit)
withSampleE = map withSample

sampleJITR :: forall s m a b n. Korok s m => AnEvent m a -> AnEvent m (ReaderT (AVar.AVar a) n b) -> AnEvent m (n b)
sampleJITR e ae = fromEvent $ sampleJIT (toEvent e) (toEvent (unwrap <$> ae))

sampleJITE :: forall s m a n. MonadEffect n => Korok s m => AnEvent m a -> AnEvent m (ReaderT a n Unit) -> AnEvent m (n Unit)
sampleJITE e ae = fromEvent $ sampleJIT (toEvent e) (toEvent (withSample2 <$> ae))

class ReadersT i o where
  readersT :: i -> o

instance moreReadersT :: ReadersT o (m y) => ReadersT (i -> o) (ReaderT i m y) where
  readersT = ReaderT <<< map readersT
else instance noReadersT :: TypeEquals o (m x) => ReadersT o (m x) where
  readersT = to
