module Parser.Comb.Dragon where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested ((/\))
import Idiolect (intercalateMap)
import Parser.Comb.Comber (FullParseError)
import Parser.Lexing (FailReason(..), FailedStack(..), Similar(..), errorName, len, userErrors)
import Parser.Types (ShiftReduce(..), unShift)
import Parser.Types as P
import Partial.Unsafe (unsafeCrashWith)
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones ((.$), (.$~~))
import Riverdragon.Dragon.Bones as D


renderParseError :: FullParseError -> Dragon
renderParseError theError = case theError of
  CrashedStack s -> D.div .$ D.text "Internal parser error: " <> D.text s
  FailedStack info@{ lookupState, initialInput, currentInput, failedStack } ->
    D.div.$~~
      [ case info.failReason of
          StackInvariantFailed -> D.text $ "Internal parser error: Stack invariant failed"
          UnknownState { state } -> D.text $ "Internal parser error: Unknown state " <> show state
          Uhhhh { token: cat } -> D.text $ "Internal parser error: Uhhhh " <> show cat
          UnknownReduction { reduction: nt /\ r } -> D.text $ "Internal parser error: UnknownReduction " <> show nt <> "#" <> show r
          MetaFailed {} -> D.text $ "Interal parser error: Meta failed"

          UserRejection { userErr } -> fold
            [ D.text "Parse error: User rejection"
            , listing userErr D.text
            ]
          NoTokenMatches { advance } -> fold
            [ D.text "Parse error: No token matches"
            , D.text ", expected"
            , D.text case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
                showCat cat
            ]
          UnexpectedToken { advance, matched }
            | [ _meta /\ _air /\ cat0 /\ o /\ _i ] <- NEA.toArray matched -> fold
            [ D.text "Parse error: Unexpected token "
            , showCatTok cat0 o
            , D.text ", expected"
            , D.text case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
                showCat cat
            ]
          UnexpectedToken { advance, matched: matched } -> fold
            [ D.text "Parse error: Unexpected+ambiguous token "
            , listing (NEA.toArray matched) \(_meta /\ _air /\ cat0 /\ o /\ _i) ->
                showCatTok cat0 o
            , D.text "\nExpected"
            , D.text case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
                showCat cat
            ]
          AllActionsRejected { possibilities } -> fold
            [ D.text "Parse error: All actions rejected"
            , listing (NEA.toArray possibilities) \(poss /\ errs) -> fold
                [ D.text (show poss)
                , listing errs \err -> fold
                    [ D.text $ errorName err
                    , listing (userErrors err) D.text
                    ]
                ]
            ]
          Ambiguity { filtered, userErr } -> fold
            [ D.text "Parse error: LR(1) ambiguity"
            , listing (NEA.toArray filtered) \(Tuple sr _) -> fold
                [ case sr of
                    Shift s -> D.text $ "Shift to " <> show s
                    ShiftReduces s rs -> D.text ("Shift to " <> show s <> ", or reduce ") <>
                      intercalateMap (D.text " or ") (uncurry showNTR <<< snd) rs
                    Reduces rs -> D.text "Reduce " <>
                      intercalateMap (D.text " or ") (uncurry showNTR <<< snd) rs
                , listing (Array.fromFoldable (unShift sr >>= lookupState)) \state ->
                    unwrap state.items # foldMap \item -> fold
                      [ showNTR item.pName item.rName
                      , D.text " = "
                      , case item.rule of
                          P.Zipper parsed toParse -> fold
                            [ intercalateMap (D.text " ") part parsed
                            , D.text " • "
                            , intercalateMap (D.text " ") part toParse
                            ]
                      ]
                ]
            , listing userErr D.text
            ]
      , D.text $ "\nat character " <> show (1 + len initialInput - len currentInput)
      , D.text case initialInput of
          P.Continue s -> "\n  " <> show (String.drop (len initialInput - len currentInput) s)
          P.EOF -> ""
      -- , "\n"
      -- , show info.failedState
      -- , "\n"
      -- , "Stack: " <> show (stackSize failedStack)
      ]
  where
  listing :: forall a. Array a -> (a -> Dragon) -> Dragon
  listing items f = D.ul.$~~ items <#> f >>> D.li[]

  showNTR (Left x) Nothing = D.text (x <> "#")
  showNTR (Right x) (Just r) = D.text (x <> "#" <> show r)
  showNTR _ _ = D.text "???"

  showCat = part <<< P.Terminal
  showCatTok = case _, _ of
    P.EOF, P.EOF -> D.text "$ (EOF)"
    P.Continue (Similar (Left s)), P.Continue s' | s == s' -> D.text (show s)
    P.Continue (Similar (Right r)), P.Continue s -> D.text (show s <> " (" <> show r <> ")")
    _, _ -> D.text "??"

  part = case _ of
    P.NonTerminal (Right v) -> D.text v
    P.NonTerminal (Left v) -> D.text (v <> "#")
    P.Terminal P.EOF -> D.text "$"
    P.Terminal (P.Continue (Similar (Left s))) -> D.text (show s)
    P.Terminal (P.Continue (Similar (Right r))) -> D.text (show r)
    P.InterTerminal _ -> unsafeCrashWith "Comb.part"

