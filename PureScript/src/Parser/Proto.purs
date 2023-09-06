module Parser.Proto where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Stack state tok
  = Zero state
  | Snoc (Stack state tok) tok state

derive instance functorStack :: Functor (Stack state)
derive instance eqStack :: (Eq state, Eq tok) => Eq (Stack state tok)
derive instance ordStack :: (Ord state, Ord tok) => Ord (Stack state tok)
derive instance genericStack :: Generic (Stack state tok) _
instance showStack :: (Show state, Show tok) => Show (Stack state tok) where
  show x = genericShow x

topOf :: forall state tok. Stack state tok -> state
topOf (Zero state) = state
topOf (Snoc _ _ state) = state

newtype Table state rule i o = Table
  { step :: state -> i -> Maybe (Either state rule)
  , promote :: i -> o
  , goto :: rule -> Stack state o -> Maybe (Stack state o)
  }

parseOne :: forall state rule i o. Table state rule i o -> i -> Stack state o -> Maybe (Stack state o)
parseOne table@(Table { step, promote, goto }) input stack = step (topOf stack) input >>= takeAction where
  takeAction (Left newState) = Just (Snoc stack (promote input) newState)
  takeAction (Right rule) = goto rule stack >>= parseOne table input

parseMany :: forall state rule i o. Table state rule i o -> List i -> state -> Maybe (Stack state o)
parseMany table inputs initialState = go inputs (Zero initialState) where
  go Nil output = Just output
  go (i : is) stack = parseOne table i stack >>= go is

type ParseStep input stack = { stack :: stack, inputs :: List input }
data ParseSteps rule input stack
  = Error (ParseStep input stack)
  | Complete (ParseStep input stack) stack
  | Step (ParseStep input stack) (Either input rule) (ParseSteps rule input stack)
derive instance functorParseSteps :: Functor (ParseSteps rule input)

startParse :: forall state i o. List i -> state -> ParseStep i (Stack state o)
startParse inputs initialState = { stack: Zero initialState, inputs: inputs }

parseStep :: forall state rule i o.
  Table state rule i o ->
  (ParseStep i (Stack state o)) ->
  Either (Maybe (Stack state o)) { next :: ParseStep i (Stack state o), action :: Either i rule }
parseStep (Table { step, promote, goto }) { inputs, stack } =
  case inputs of
    Nil -> Left (Just stack)
    input : inputs' -> case step (topOf stack) input of
      Nothing -> Left Nothing
      Just (Left newState) ->
        let stack' = (Snoc stack (promote input) newState)
        in Right { next: { stack: stack', inputs: inputs' }, action: Left input }
      Just (Right rule) ->
        case goto rule stack of
          Nothing -> Left Nothing
          Just stack' -> Right { next: { stack: stack', inputs: inputs }, action: Right rule }

parseSteps :: forall state rule i o. Table state rule i o -> List i -> state -> ParseSteps rule i (Stack state o)
parseSteps table inputs initialState =
  let step = startParse inputs initialState in
  go step where
    go step = case parseStep table step of
      Left Nothing -> Error step
      Left (Just v) -> Complete step v
      Right { next: step', action } -> Step step action (go step')

parse :: forall f state rule i o. Foldable f => Table state rule i o -> state -> f i -> Maybe (Stack state o)
parse table initialState inputs = parseSteps table (fromFoldable inputs) initialState # finished

finished :: forall rule i stack. ParseSteps rule i stack -> Maybe stack
finished (Step _ _ more) = finished more
finished (Error _) = Nothing
finished (Complete _ stack) = Just stack
