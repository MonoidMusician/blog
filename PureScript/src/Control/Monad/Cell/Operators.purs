module Control.Monad.Cell.Operators where

import Prelude

import Control.Monad.Cell.Basics (Modify(..))
import Data.Tuple (Tuple(..))


ofCell :: forall ops m v r. Functor m => (v -> r) -> { get :: m v | ops } -> m r
ofCell f = map f <<< _.get
infix 1 ofCell as $&

fromCell :: forall ops m v r. Functor m => { get :: m v | ops } -> (v -> r) -> m r
fromCell = flip ofCell
infix 1 fromCell as &#

setCell :: forall ops m v r. Functor m => { set :: v -> m r | ops } -> v -> m Unit
setCell = map void <<< _.set
infix 1 setCell as &=

swapCell :: forall ops m v r. { swap :: v -> m r | ops } -> v -> m r
swapCell = _.swap
infix 1 swapCell as <&=

updateCell :: forall ops m v r. Functor m => { update :: (v -> v) -> m r | ops } -> (v -> v) -> m Unit
updateCell = map void <<< _.update
infix 1 updateCell as &~

swapifyCell :: forall ops m v. Functor m => { modify :: Modify m v | ops } -> (v -> v) -> m v
swapifyCell { modify: Modify modify } fn = _.info <$> modify \prev -> Tuple (fn prev) prev
infix 1 swapifyCell as <&~

appendCell :: forall ops m v r. Functor m => Monoid v => { update :: (v -> v) -> m r | ops } -> v -> m Unit
appendCell { update } value = void do update (_ <> value)
infix 1 appendCell as &<>

