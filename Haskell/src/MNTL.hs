{-# LANGUAGE QuantifiedConstraints, ScopedTypeVariables #-}
{- HLINT ignore "Eta reduce" -}
module MNTL where

import Control.Monad.State (StateT (StateT, runStateT), MonadState (get, put), mapStateT, evalStateT)
import Control.Monad.Trans.Except (ExceptT (ExceptT), mapExceptT, runExceptT, Except)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad (join)
import Data.Coerce (coerce, Coercible)
import Data.Bifunctor (Bifunctor(bimap, first))
import Control.Monad.Trans.Writer (WriterT (WriterT, runWriterT), mapWriterT, Writer)
import Data.Functor.Compose (Compose (Compose))
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), mapReaderT, Reader)
import GHC.Base (Type)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Tuple (swap)
import Control.Monad.Trans.Identity (IdentityT (IdentityT), mapIdentityT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), mapMaybeT)

-- | A class that exposes effects from a monad transformer `n` from *somewhere*
-- within the monad `m`, pulling it out of as many layers as necessary.
-- This is enough to emulate `MonadState s` with `Exposes (StateT s)` and so on.
--
-- No fundep! Avoids the m*n problem by using magic.
class (Functor1 n, Functor m) => Exposes n m where
  expose :: m x -> n m x
  incorporate :: n m x -> m x

-- | Functor lifted to monad-transformer-likes. Unfortunately some transformers,
-- like `ContT` and `LogicT`, are not covariant in their underlying monad `m`.
class (forall m. Functor m => Functor (n m)) => Functor1 n where
  hoist :: forall m1 m2. Functor m1 => Functor m2 => (forall y. m1 y -> m2 y) -> (forall x. n m1 x -> n m2 x)

-- | You can join two layers of transformers, *but* it is not pretty: it has to
-- prefer effects from the outer layer (e.g. for state and writer in particular).
class Functor1 n => Monadic n where
  jojoin :: forall m x. Functor m => n (n m) x -> n m x
  lift :: forall m x. Functor m => m x -> n m x

-- Functor composition
newtype Precomp g f a = Precomp (f (g a))
  deriving (Functor)
newtype Prepost f g h a = Prepost (f (h (g a)))
  deriving (Functor)

instance Monadic (StateT s) where
  jojoin (StateT nmx) = StateT \s -> do
    -- Keep the *outer* state, I guess
    fst <$> runStateT (nmx s) s
  lift mx = StateT \s -> (,s) <$> mx

instance Monoid w => Monadic (WriterT w) where
  jojoin (WriterT nmx) = WriterT $
    -- Keep the *outer* state, I guess
    fst <$> runWriterT nmx
  lift mx = WriterT $ (,mempty) <$> mx
instance Monadic (ReaderT r) where
  jojoin (ReaderT nmx) = ReaderT \r -> runReaderT (nmx r) r
  lift mx = ReaderT (const mx)

instance Monadic (ExceptT e) where
  jojoin (ExceptT (ExceptT inner)) = ExceptT $ either Left id <$> inner
  lift mx = ExceptT $ Right <$> mx
instance Monadic MaybeT where
  jojoin (MaybeT (MaybeT inner)) = MaybeT $ join @Maybe <$> inner
  lift mx = MaybeT $ Just <$> mx
instance Monadic IdentityT where
  jojoin = coerce
  lift = coerce


-- | A monad transformer can be regarded as a composition of functors.
class (Functor1 t, Functor (StackBefore t), Functor (StackAfter t)) => Stack t where
  type StackBefore t :: (Type -> Type)
  type StackAfter t :: (Type -> Type)
  -- These need to form an isomorphism.
  toStack :: forall m x. Functor m => t m x -> Prepost (StackAfter t) (StackBefore t) m x
  fromStack :: forall m x. Functor m => Prepost (StackAfter t) (StackBefore t) m x -> t m x

type AsStack t = Prepost (StackAfter t) (StackBefore t)

instance Stack (StateT s) where
  type StackBefore (StateT s) = (,) s
  type StackAfter (StateT s) = (->) s
  toStack (StateT f) = Prepost (fmap swap . f)
  fromStack (Prepost f) = StateT (fmap swap . f)

instance Stack (ReaderT r) where
  type StackBefore (ReaderT r) = Identity
  type StackAfter (ReaderT r) = Reader r
  toStack (ReaderT f) = Prepost (ReaderT (Identity . fmap coerce . f))
  fromStack (Prepost (ReaderT f)) = ReaderT (fmap coerce . runIdentity . f)

instance Stack (WriterT w) where
  type StackBefore (WriterT w) = Writer w
  type StackAfter (WriterT w) = Identity
  toStack (WriterT m) = Prepost (coerce $ fmap (WriterT . Identity) m)
  fromStack (Prepost m) = WriterT (runIdentity . runWriterT <$> coerce m)

instance Stack (ExceptT e) where
  type StackBefore (ExceptT e) = Either e
  type StackAfter (ExceptT e) = Identity
  toStack (ExceptT m) = Prepost (coerce m)
  fromStack (Prepost m) = ExceptT (coerce m)
instance Stack MaybeT where
  type StackBefore MaybeT = Maybe
  type StackAfter MaybeT = Identity
  toStack (MaybeT m) = Prepost (coerce m)
  fromStack (Prepost m) = MaybeT (coerce m)
instance Stack IdentityT where
  type StackBefore IdentityT = Identity
  type StackAfter IdentityT = Identity
  toStack m = coerce $ fmap Identity m
  fromStack = fmap runIdentity . coerce


-- | Any monad transformer can tell us how it can exchange with other functors.
-- This is not an isomorphism.
class Functor1 n => ExchangeVia cf cg n | n -> cf cg where
  exchangeAny :: forall f g m x. cf f => cg g => Functor m => Prepost f g (n m) x -> n (Prepost f g m) x

-- | Finally we can put this together in this single-instance class to exchange
-- any two monad transformers. Again, this is not an isomorphism, but we do
-- expect that layers without effects will perfectly commute with others.
class (Functor1 t, Functor1 n, Exchange n t) => Exchange t n where
  -- | lift @t = exchange . hoist (lift @t)
  -- | hoist (lift @n) = exchange . lift @n
  exchange :: forall m x. Functor m => t (n m) x -> n (t m) x

_1, _2 :: forall n t m x. Exchange n t => Monadic n => Functor m => t m x -> t (n m) x
_1 = exchange . lift @n
_2 = hoist (lift @n)

_3, _4 :: forall n t m x. Exchange n t => Monadic t => Functor m => n m x -> t (n m) x
_3 = exchange . hoist (lift @t)
_4 = lift @t

-- The single instance
instance
  ( Stack t
  , cf (StackAfter t)
  , cg (StackBefore t)
  , ExchangeVia cf cg n
  , Functor1 t
  , Functor1 n

  , Stack n
  , df (StackAfter n)
  , dg (StackBefore n)
  , ExchangeVia df dg t
  ) => Exchange t n where
  exchange :: forall m x. Functor m => t (n m) x -> n (t m) x
  exchange = hoist fromStack . exchangeAny . toStack

instance ExchangeVia Functor Functor (StateT s) where
  exchangeAny :: forall f g m x. (Functor f, Functor g, Functor m) => Prepost f g (StateT s m) x -> StateT s (Prepost f g m) x
  exchangeAny (Prepost stack) = StateT \s -> Prepost $
    fmap (\(g, s') -> (,s') <$> g) . (`runStateT` s) <$> stack

instance ExchangeVia Functor Functor (WriterT w) where
  exchangeAny :: forall f g m x. (Functor f, Functor g, Functor m) => Prepost f g (WriterT w m) x -> WriterT w (Prepost f g m) x
  exchangeAny (Prepost stack) = WriterT $ Prepost $
    fmap (\(g, w) -> (,w) <$> g) . runWriterT <$> stack

instance ExchangeVia Functor Functor (ReaderT r) where
  exchangeAny :: forall f g m x. (Functor f, Functor g, Functor m) => Prepost f g (ReaderT r m) x -> ReaderT r (Prepost f g m) x
  exchangeAny (Prepost stack) = ReaderT \r -> Prepost $
    (`runReaderT` r) <$> stack

instance ExchangeVia Functor Applicative (ExceptT e) where
  exchangeAny :: forall f g m x. (Functor f, Applicative g, Functor m) => Prepost f g (ExceptT e m) x -> ExceptT e (Prepost f g m) x
  exchangeAny (Prepost stack) = ExceptT $ Prepost $
    fmap (either (pure . Left) (fmap Right)) . runExceptT <$> stack

-- This is the base instance, equivalent to `MonadState s (StateT s m)`
instance {-# OVERLAPPING #-} (s' ~ s, Functor m) => Exposes (StateT s') (StateT s m) where
  expose (StateT comp) = StateT \s ->
    StateT \_ -> (\(x, s') -> ((x, s'), s')) <$> comp s
  incorporate (StateT comp) = StateT \s ->
    fst <$> runStateT (comp s) s

-- For any other effects, they must be found within `m`, not `StateT`.
instance {-# OVERLAPPABLE #-} (Exchange n (StateT s), Exchange (StateT s) n, Exposes n m) => Exposes n (StateT s m) where
  expose = exchange @(StateT s) @n . hoist @(StateT s) (expose @n @m)
  incorporate = hoist @(StateT s) (incorporate @n @m) . exchange @n @(StateT s)

-- (m <~> n m) -> (StateT s m <~> n (StateT s m))
-- StateT s m ~> StateT s (n m) ~> n (StateT s m)
-- n (StateT s m) ~> StateT s (n m) ~> StateT s m

instance {-# OVERLAPPING #-} (w' ~ w, Functor m) => Exposes (WriterT w') (WriterT w m) where
  expose (WriterT comp) = WriterT $ WriterT $ (\(x, w) -> ((x, w), w)) <$> comp
  incorporate (WriterT comp) = WriterT $ fst <$> runWriterT comp

instance {-# OVERLAPPABLE #-} (Exposes n m, Exchange n (WriterT w)) => Exposes n (WriterT w m) where
  expose = exchange @(WriterT w) @n . hoist @(WriterT w) (expose @n @m)
  incorporate = hoist @(WriterT w) (incorporate @n @m) . exchange @n @(WriterT w)

instance {-# OVERLAPPING #-} (r' ~ r, Functor m) => Exposes (ReaderT r') (ReaderT r m) where
  expose (ReaderT comp) = ReaderT \r -> ReaderT \_ -> comp r
  incorporate (ReaderT comp) = ReaderT \r -> runReaderT (comp r) r

instance {-# OVERLAPPABLE #-} (Exposes n m, Exchange n (ReaderT r)) => Exposes n (ReaderT r m) where
  expose = exchange @(ReaderT r) @n . hoist @(ReaderT r) (expose @n @m)
  incorporate = hoist @(ReaderT r) (incorporate @n @m) . exchange @n @(ReaderT r)



instance Functor g => Functor1 (Precomp g) where
  hoist f (Precomp fga) = Precomp (f fga)
instance (Functor f, Functor g) => Functor1 (Prepost f g) where
  hoist g (Prepost fga) = Prepost (g <$> fga)
instance Functor f => Functor1 (Compose f) where
  hoist g (Compose fga) = Compose (g <$> fga)

instance Functor1 (StateT s) where
  hoist f = mapStateT f
instance Functor1 (WriterT w) where
  hoist f = mapWriterT f
instance Functor1 (ReaderT r) where
  hoist f = mapReaderT f
instance Functor1 (ExceptT e) where
  hoist f = mapExceptT f
instance Functor1 MaybeT where
  hoist f = mapMaybeT f
instance Functor1 IdentityT where
  hoist f = mapIdentityT f

