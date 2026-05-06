-- @inline export mintCell always
-- @inline export cellularEffect._mintCell arity=1
-- @inline export cellularST._mintCell arity=1
-- @inline export cellularAff._mintCell arity=1
module Control.Monad.Cell.Basics where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, class MonadTrans, ReaderT(..), ask, local)
import Control.Monad.ST (ST)
import Control.Monad.ST.Internal as STRef
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Either (Either(..))
import Data.Pair (Pair(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref


-- | This is an abstract interface to a simple mutable cell.
type Cell m v =
  { get :: m v
  , set :: v -> m Unit
  , swap :: v -> m v
  , update :: (v -> v) -> m (Pair v)
  , modify :: Modify m v
  }
newtype Modify m v = Modify (forall r. (v -> Tuple v r) -> m { prev :: v, next :: v, info :: r })

liftCell :: forall @m @n v. (m ~> n) -> Cell m v -> Cell n v
liftCell morph cell =
  { get: cell.get # morph
  , set: cell.set >>> morph
  , swap: cell.swap >>> morph
  , update: cell.update >>> morph
  , modify: case cell.modify of Modify modify -> Modify (modify >>> morph)
  }

basicCell :: forall m v r. Monad m => { get :: m v, set :: v -> m Unit | r } -> Cell m v
basicCell { get, set } =
  { get, set
  , swap: \v -> get <* set v
  , update: \f -> do
      prev <- get
      let next = f prev
      set next
      pure $ Pair prev next
  , modify: Modify \f -> do
      prev <- get
      let Tuple next info = f prev
      set next
      pure { prev, next, info }
  }

mapCell :: forall m v v'. Functor m => (v' -> v) -> (v -> v') -> Cell m v -> Cell m v'
mapCell v'v vv' cell =
  { get: vv' <$> cell.get
  , set: v'v >>> cell.set
  , swap: v'v >>> cell.swap >>> map vv'
  , update: \f -> _.info <$> modify \prev ->
      let
        prev' = vv' prev
        next' = f prev'
        next = v'v next'
      in Tuple next (Pair prev' next')
  , modify: Modify \f -> _.info <$> modify \prev ->
      let
        prev' = vv' prev
        Tuple next' info = f prev'
        next = v'v next'
      in Tuple next { prev: prev', next: next', info }
  }
  where Modify modify = cell.modify



-- | Make a mutable cell, appropriate for the effect type (Ref or STRef)
class Monad m <= Cellular m where
  _mintCell :: forall @v. v -> m (Cell m v)

mintCell :: forall @v @m. Cellular m => v -> m (Cell m v)
mintCell = _mintCell @m @v

instance cellularEffect :: Cellular Effect where
  _mintCell = Ref.new >>> map \ref ->
    { get: Ref.read ref
    , set: Ref.write <@> ref
    , swap: \v -> modify' (\value -> { state: v, value }) ref
    , update: \f -> modify' (\prev -> let next = f prev in { state: next, value: Pair prev next }) ref
    , modify: Modify \f -> modify' (\prev -> let Tuple next info = f prev in { state: next, value: { prev, next, info } }) ref
    }
    where
    modify' :: forall s r. (s -> { state :: s, value :: r }) -> Ref.Ref s -> Effect r
    modify' f ref = do
      -- purescript-backend-es does not optimize Ref.modify'
      { state, value } <- f <$> Ref.read ref
      value <$ Ref.write state ref

instance cellularST :: Cellular (ST r) where
  _mintCell = STRef.new >>> map \ref ->
    { get: STRef.read ref
    , set: (STRef.write <@> ref) >>> void
    , swap: \v -> STRef.modify' (\value -> { state: v, value }) ref
    , update: \f -> STRef.modify' (\prev -> let next = f prev in { state: next, value: Pair prev next }) ref
    , modify: Modify \f -> STRef.modify' (\prev -> let Tuple next info = f prev in { state: next, value: { prev, next, info } }) ref
    }

instance cellularAff :: Cellular Aff where
  _mintCell = _mintCell >>> liftEffect >>> map (liftCell liftEffect)



-- | Use a cell for a mutable state transformer.
newtype Cellfie v m a = Cellfie (ReaderT (Cell m v) m a)

persistentState :: forall v m i o. Cellular m =>
  v ->
  (i -> Cellfie v m o) ->
  m (i -> m o)
persistentState initial run = do
  mintCell initial <#> \cell input -> case run input of
    Cellfie (ReaderT withCell) -> withCell cell

runCellfie :: forall v m a. Cell m v -> Cellfie v m a -> m a
runCellfie cell (Cellfie (ReaderT f)) = f cell


-- | Really basic async capabilities.
class Monad m <= BasicAsync m where
  -- | Schedule an event to run _soon_ (fork). Returns synchronously.
  schedule :: m Unit -> m Unit
  -- | Wait on a callback to provide a result, one time only.
  waiting :: forall o. ((o -> m Unit) -> m Unit) -> m o

instance BasicAsync Aff where
  schedule = void <<< forkAff
  waiting withCb = Aff.makeAff \cb -> do
    running <- Aff.launchAff do
      withCb \o -> liftEffect do
        cb (Right o)
    pure $ Aff.fiberCanceler running


-- | Runs two actions concurrently, but waits for them to both return before
-- | returning a composite result, allowing an effect in the meantime.
-- | Only one of the continuations `m (y -> r)` or `m (x -> r)` is run,
-- | they are not raced even if both `x` and `y` complete before the
-- | continuation does.
sideBySide :: forall m x y r. Cellular m => BasicAsync m =>
  m (Tuple x (m (y -> r))) ->
  m (Tuple y (m (x -> r))) ->
  m r
sideBySide produceX produceY = do
  cell <- mintCell (HasNeither :: SideBySideState x y r)
  waiting \fulfill -> do
    schedule do
      Tuple x produceWithY <- produceX
      cell.get >>= case _ of
        HasNeither -> do
          cell.set (HasX x)
          withY <- produceWithY
          cell.get >>= case _ of
            HasXY y -> do
              cell.set Done
              fulfill (withY y)
            HasX _ -> do
              cell.set (XAwaitingY withY)
            _ -> pure unit
        HasY y -> do
          withY <- produceWithY
          fulfill (withY y)
        _ -> pure unit
    schedule do
      Tuple y produceWithX <- produceY
      cell.get >>= case _ of
        HasNeither -> do
          cell.set (HasY y)
          withX <- produceWithX
          cell.get >>= case _ of
            HasYX x -> do
              cell.set Done
              fulfill (withX x)
            HasY _ -> do
              cell.set (YAwaitingX withX)
            _ -> pure unit
        HasX x -> do
          withX <- produceWithX
          fulfill (withX x)
        _ -> pure unit

-- `HasNeither -> HasX -> XAwaitingY -> Done`
-- `HasNeither -> HasX -> HasXY -> Done`
data SideBySideState x y r
  = HasNeither
  | HasX x
  | HasY y
  | XAwaitingY (y -> r)
  | YAwaitingX (x -> r)
  | HasXY y
  | HasYX x
  | Done




derive newtype instance functorCellfie :: Functor m => Functor (Cellfie v m)
derive newtype instance applyCellfie :: Monad m => Apply (Cellfie v m)
derive newtype instance applicativeCellfie :: Monad m => Applicative (Cellfie v m)
derive newtype instance bindCellfie :: Monad m => Bind (Cellfie v m)
derive newtype instance monadCellfie :: Monad m => Monad (Cellfie v m)

derive newtype instance monadEffectCellfie :: MonadEffect m => MonadEffect (Cellfie v m)
derive newtype instance monadAffCellfie :: MonadAff m => MonadAff (Cellfie v m)
derive newtype instance monadTellCellfie :: MonadTell w m => MonadTell w (Cellfie v m)
derive newtype instance monadWriterCellfie :: MonadWriter w m => MonadWriter w (Cellfie v m)
derive newtype instance monadThrowCellfie :: MonadThrow e m => MonadThrow e (Cellfie v m)
derive newtype instance monadErrorCellfie :: MonadError e m => MonadError e (Cellfie v m)
instance monadAskCellfie :: MonadAsk r m => MonadAsk r (Cellfie v m) where
  ask = Cellfie (ReaderT \_ -> ask)
instance monadReaderCellfie :: MonadReader r m => MonadReader r (Cellfie v m) where
  local f (Cellfie (ReaderT sma)) = Cellfie (ReaderT (local f <<< sma))

instance monadTransCellfie :: MonadTrans (Cellfie v) where
  lift = Cellfie <<< ReaderT <<< const

hoistCellfie :: forall v m n. Monad n => (m ~> n) -> n (n ~> m) -> Cellfie v m ~> Cellfie v n
hoistCellfie up getDown (Cellfie (ReaderT withCell)) = Cellfie $ ReaderT
  \cell -> do
    down <- getDown
    up $ withCell $ liftCell down cell

instance monadStateCellfie :: Monad m => MonadState s (Cellfie s m) where
  state f = Cellfie $ ReaderT \cell -> do
    v <- cell.get
    let Tuple r v' = f v
    r <$ cell.set v'

derive newtype instance semigroupCellfie :: (Semigroup a, Apply m) => Semigroup (Cellfie v m a)
derive newtype instance monoidCellfie :: (Monoid a, Applicative m) => Monoid (Cellfie v m a)
