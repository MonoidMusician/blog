module Lz where

import Prelude

import Control.Lazy (class Lazy, defer)
import Data.Lazy (Lazy, force)
import Data.List.Lazy (List(..), cons, head)
import Data.Maybe (fromMaybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))

unfirst :: forall a b c. ((a /\ Lazy c) -> (b /\ Lazy c)) -> a -> b
unfirst f a =
  let
    r :: Lazy (b /\ Lazy c)
    r = defer \_ -> pure (f (a /\ c))
    c :: Lazy c
    c = defer \_ -> r >>= snd
  in force (fst <$> r)


unfirst' :: forall a b c. Lazy c => ((a /\ c) -> (b /\ c)) -> a -> b
unfirst' f a =
  let
    r :: (b /\ c)
    r = f (a /\ defer \_ -> c)
    c :: c
    c = defer \_ -> snd r
  in fst r

inf :: Int /\ (Lazy (List Int)) -> Int /\ (Lazy (List Int))
inf (a /\ c) = (a) /\ (cons a <$> c)

inf2 :: Int /\ (List Int) -> Int /\ (List Int)
inf2 (a /\ c) = (a) /\ (cons a c)
