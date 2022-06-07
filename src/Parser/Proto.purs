module Parser.Proto where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Stack state tok
  = Zero state
  | Snoc (Stack state tok) tok state

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
data ParseSteps input stack
  = Error
  | Complete stack
  | Step (ParseStep input stack) (ParseSteps input stack)

startParse :: forall state i o. List i -> state -> ParseStep i (Stack state o)
startParse inputs initialState = { stack: Zero initialState, inputs: inputs }

parseStep :: forall state rule i o.
  Table state rule i o ->
  (ParseStep i (Stack state o)) ->
  Either (Maybe (Stack state o)) (ParseStep i (Stack state o))
parseStep (Table { step, promote, goto }) { inputs, stack } =
  case inputs of
    Nil -> Left (Just stack)
    input : inputs' -> case step (topOf stack) input of
      Nothing -> Left Nothing
      Just (Left newState) ->
        let stack' = (Snoc stack (promote input) newState)
        in Right { stack: stack', inputs: inputs' }
      Just (Right rule) ->
        case goto rule stack of
          Nothing -> Left Nothing
          Just stack' -> Right { stack: stack', inputs: inputs }

parseSteps :: forall state rule i o. Table state rule i o -> List i -> state -> ParseSteps i (Stack state o)
parseSteps table inputs initialState =
  let step = startParse inputs initialState in
  Step step (go step) where
    go step = case parseStep table step of
      Left Nothing -> Error
      Left (Just v) -> Complete v
      Right step' -> Step step' (go step')
