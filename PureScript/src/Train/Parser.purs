module Train.Parser where

import Prelude
import Train.Types (Command(..), Direction(..), TrainUnit(..))

import Control.Plus (empty)
import Data.Array as Array
import Data.CodePoint.Unicode (isAsciiUpper, isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (all, fold)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.Profunctor (dimap)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Tuple (Tuple(..))

parseTraintle :: String -> Either String (Array Command)
parseTraintle =
  String.split (String.Pattern "")
  >>> dimap Array.toUnfoldable (map Array.fromFoldable) parseCommands

parseCommands :: List String -> Either String (List Command)
parseCommands s = s # parseCommandsWith (Right Nil)
  \s' -> Left $ "Error at: " <> show (String.joinWith "" (Array.fromFoldable s'))


-- | A simple CPS-style parser.
parseCommandsWith :: forall f. Functor f => f (List Command) -> (List String -> f (List Command)) -> List String -> f (List Command)
parseCommandsWith finish continue (" " : s) = parseCommandsWith finish continue s
parseCommandsWith finish continue ("\n" : s) = parseCommandsWith finish continue s
parseCommandsWith finish continue ("\t" : s) = parseCommandsWith finish continue s
-- # comment
parseCommandsWith finish continue ("#" : s) = parseCommandsWith finish continue $ List.dropWhile (_ /= "\n") s
-- q/e: shallow turns
-- w/s: forward/back
-- a/d: sharp turns
-- x: reverse direction
parseCommandsWith finish continue ("q" : s) = (Q : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("w" : s) = (W : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("e" : s) = (E : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("a" : s) = (A : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("s" : s) = (S : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("d" : s) = (D : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("z" : s) = (Z : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("x" : s) = (X : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("c" : s) = (C : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("u" : s) = (Silent (pure Q) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("i" : s) = (Silent (pure W) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("o" : s) = (Silent (pure E) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("j" : s) = (Silent (pure A) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("k" : s) = (Silent (pure S) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("l" : s) = (Silent (pure D) : _) <$> parseCommandsWith finish continue s
parseCommandsWith finish continue ("r" : s)
  | digits <- List.takeWhile (all isDecDigit <<< String.toCodePointArray) s
  , Just num <- Int.fromString (fold digits)
  = (SetRadius (num-1) : _) <$> parseCommandsWith finish continue
      (List.drop (List.length digits) s)
-- @NAME{...}: drawing subroutine
parseCommandsWith finish continue ("@" : s0)
  | Tuple name s1 <- parseNAME $ skipWS s0 =
    case skipWS s1 of
      "{" : s2 -> let
        exitScope = case _ of
          "}" : s3 -> Tuple (Just s3) Nil
          _ -> Tuple Nothing Nil
        in case parseCommandsWith (Tuple (Just Nil) Nil) exitScope s2 of
          Tuple Nothing _ -> continue ("@" : s0)
          Tuple (Just s3) cmds ->
            (Subroutine name (Just (List.toUnfoldable cmds)) : _) <$> parseCommandsWith finish continue s3
      _ -> (Subroutine name Nothing : _) <$> parseCommandsWith finish continue s1
-- ~NAME(...){...}: train and route
--     <==:{=}:{%}:{=}:<==
--     <======={======}{======}{======}=======>
parseCommandsWith finish continue ("~" : s0)
  | Tuple name s1 <- parseNAME $ skipWS s0
  , Tuple train s2 <- parseTrain $ skipWS s1
  , Tuple route s3 <- parseRoute $ skipWS s2
  =
    (TrainRoute name (maybe [] List.toUnfoldable train) (List.toUnfoldable route) : _) <$>
      parseCommandsWith finish continue s3
-- &NAME{...}: route
parseCommandsWith finish continue ("&" : s0)
  | Tuple name s1 <- parseNAME $ skipWS s0
  , Tuple route s2 <- parseRoute $ skipWS s1
  =
    (TrainRoute name [] (List.toUnfoldable route) : _) <$>
      parseCommandsWith finish continue s2
-- {...}: logic?
-- <x,y>, <x,y:>, <:dx,dy>, <x,y:dx,dy>: relative teleport to:facing
-- $var: procedure variables?
-- *var: multiplicative variables?
-- 0-9, 0-9(...): repeat
-- (?...): without writing paths
parseCommandsWith finish continue ("(" : inner)
  = parseCommandsWith finish continue ("1" : "(" : inner)
parseCommandsWith finish continue s
  | digits <- List.takeWhile (all isDecDigit <<< String.toCodePointArray) s
  , Just num <- Int.fromString (fold digits) =
    case skipWS $ List.drop (List.length digits) s of
      "(" : questionInner -> let
        exitScope = case _ of
          ")" : more -> Tuple (Just (Tuple num more)) Nil
          _ -> Tuple Nothing Nil
        Tuple question inner = case questionInner of
          "?" : inner -> Tuple ((\cmds -> (Silent (List.toUnfoldable cmds) : _))) inner
          inner -> Tuple (<>) inner
        in case parseCommandsWith (Tuple (Just (Tuple 1 mempty)) Nil) exitScope inner of
          Tuple Nothing _ -> continue s
          Tuple (Just (Tuple repeats more)) cmds ->
            question (power cmds repeats) <$> parseCommandsWith finish continue more
      c : more ->
        case parseCommands (pure c) of
          Left _ -> continue (c : more)
          Right cmds -> (power cmds num <> _) <$> parseCommandsWith finish continue more
      Nil -> finish
-- capital, or quotes: load named place
-- =NAME: set named place
-- =: teleport to origin, of function or whole program
-- /NAME: append to named variable's stack (repeat if desired)
-- \NAME: pop from named variable's stack (teleport to location)
parseCommandsWith finish continue s
  | Tuple name more <- parseNAME s
  , name /= "" = (GetVariable Nothing name : _) <$> parseCommandsWith finish continue more
parseCommandsWith finish continue ("=" : s0)
  | Tuple name more <- parseNAME $ skipWS s0
  =
    ((if name == "" then Origin else (SetVariable Nothing name)) : _) <$> parseCommandsWith finish continue more
parseCommandsWith finish continue ("/" : s0)
  | taken <- List.length $ List.takeWhile (_ == "/") s0
  , s1 <- List.drop taken s0
  , Tuple name more <- parseNAME $ skipWS s1
  , name /= ""
  =
    (SetVariable (Just (taken + 1)) name : _) <$>
      parseCommandsWith finish continue more
parseCommandsWith finish continue ("\\" : s0)
  | taken <- List.length $ List.takeWhile (_ == "\\") s0
  , s1 <- List.drop taken s0
  , Tuple name more <- parseNAME $ skipWS s1
  , name /= ""
  =
    (GetVariable (Just (taken + 1)) name : _) <$>
      parseCommandsWith finish continue more

parseCommandsWith finish _ Nil = finish
parseCommandsWith _ continue s = continue s


skipWS :: List String -> List String
skipWS (" " : s) = skipWS s
skipWS ("\n" : s) = skipWS s
skipWS ("\t" : s) = skipWS s
skipWS ("#" : s) = skipWS $ List.dropWhile (_ /= "\n") s
skipWS s = s

-- | Parse a name: capitals and underscores, or a simple quoted literal (no escapes).
parseNAME :: List String -> Tuple String (List String)
parseNAME ("\"" : s) =
  let taken = List.takeWhile (_ /= "\"") s
  in Tuple (fold taken) (List.drop (List.length taken + 1) s)
parseNAME s =
  let taken = List.takeWhile (all (isAsciiUpper || eq (codePointFromChar '_')) <<< String.toCodePointArray) s
  in Tuple (fold taken) (List.drop (List.length taken) s)

parseTrain :: List String -> Tuple (Maybe (List TrainUnit)) (List String)
parseTrain ("(" : s0) =
  let
    go acc s1 = case parseTrainUnit $ skipWS s1 of
      Tuple Nothing s2 -> Tuple (Just acc)
        case skipWS s2 of
          ")" : s3 -> s3
          s3 -> s3
      Tuple (Just u) s2 -> go (u : acc) s2
  in go empty s0
parseTrain s = Tuple Nothing s

-- <== Locomotive Forward 3
-- ==> Locomotive Backward 3
-- <=> Locomotive Bothward 3
-- {=} Car "=" 3
parseTrainUnit :: List String -> Tuple (Maybe TrainUnit) (List String)
parseTrainUnit s0 =
  case skipWS s0 of
    "<" : s1 -> parseLoco true 1 s1
    "=" : s1 -> parseLoco false 1 s1
    "{" : s1 -> parseCar "" s1
    ":" : s1 -> parseTrainUnit s1
    _ -> Tuple Nothing s0
  where
  parseCar acc s1 =
    case s1 of
      "}" : s2 -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 2))) s2
      s2@(")" : _) -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 1))) s2
      s2@("]" : _) -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 1))) s2
      Nil -> Tuple (if acc == "" then Nothing else Just (Car acc (String.length acc + 1))) Nil
      c : s2 -> parseCar (acc <> c) s2
  parseLoco starts acc s1 =
    case s1 of
      ">" : s2 -> Tuple (Just (loco starts true (acc + 1))) s2
      "=" : s2 -> parseLoco starts (acc + 1) s2
      s2 -> Tuple (Just (loco starts false acc)) s2
  loco starts finishes = case starts, finishes of
    true, false -> Locomotive Forward
    true, true -> Locomotive Bothward
    false, true -> Locomotive Backward
    false, false -> Locomotive Bothward

parseRoute :: List String -> Tuple (List Command) (List String)
parseRoute ("{" : s0) =
  let
    go s1 = case parseCommandsWith (Tuple empty empty) (Tuple <@> Nil) s1 of
      Tuple s2 cmds -> Tuple cmds
        case skipWS s2 of
          "}" : s3 -> s3
          s3 -> s3
  in go s0
parseRoute s = Tuple empty s
