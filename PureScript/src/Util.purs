module Util (memoizeEq) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.ST as ST
import Control.Monad.ST.Global as STG
import Control.Monad.ST.Ref as STR
import Data.Array.ST as STA
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Effect.Unsafe (unsafePerformEffect)

unsafeST :: forall a. ST.ST STG.Global a -> a
unsafeST = unsafePerformEffect <<< STG.toEffect

memoizeEq :: forall a b. Eq a => (a -> b) -> (a -> b)
memoizeEq f = unsafeST do
  vs <- STA.new
  sz <- STR.new 0
  pure \a -> unsafeST do
    res <- STR.new Nothing
    idx <- STR.new 0
    ST.while (lift2 (&&) (isNothing <$> STR.read res) (lift2 (<) (STR.read idx) (STR.read sz))) do
      i <- STR.read idx
      STA.peek i vs >>= case _ of
        Just (Tuple a' r) | a' == a ->
          void $ STR.write (Just r) res
        Just _ ->
          void $ STR.modify (add 1) idx
        Nothing ->
          void $ STR.write i sz
    STR.read res >>= case _ of
      Nothing -> do
        let r = f a
        void $ STA.push (Tuple a r) vs
        r <$ STR.modify (add 1) sz
      Just r -> pure r
