module Parser.Examples where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (fromFoldable, toUnfoldable)
import Data.Foldable (intercalate, oneOf, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.String (CodePoint)
import Data.String as CP
import Data.String as String
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Parser.Comb (Comb(..), SCComb, codePoint, named, namedRec, parseString)
import Parser.Types (Fragment, Grammar(..), Part(..))

mk :: forall a. SCComb a -> (String -> Maybe a)
mk = parseString "top"

print :: forall a. SCComb a -> Effect Unit
print (Comb { grammar: MkGrammar grammar }) = grammar # traverse_ \rule ->
  log $ rule.pName <> "." <> show (snd rule.rName) <> " ::= " <> showFragment rule.rule

showFragment :: Fragment String CodePoint -> String
showFragment =
  map (map CP.singleton) >>> toUnfoldable >>> coalesce >>> fromFoldable >>>
    map showPart >>> intercalate " "

showPart :: Part String String -> String
showPart (Terminal tok) = "\"" <> tok <> "\""
showPart (NonTerminal nt) = nt

coalesce :: List (Part String String) -> List (Part String String)
coalesce (Terminal x : Terminal y : zs) = coalesce (Terminal y : zs)
coalesce (z : zs) = z : coalesce zs
coalesce Nil = Nil

digit :: SCComb Int
digit = named "digit" $ oneOf $ mapWithIndex (<$) $
  codePoint <$> String.toCodePointArray "0123456789"

number :: SCComb Int
number = namedRec "number" \numberRec ->
  digit <|> ado
    ds <- numberRec
    d <- digit
    in 10*ds + d

testData :: Array String
testData =
  [ ""
  , "0"
  , "1"
  , "10"
  , "01"
  , "1234"
  ]

test :: forall a. Show a => SCComb a -> Effect Unit
test parser = do
  print parser
  let doParse = mk parser
  traverse_ (\s -> logShow (s /\ doParse s)) testData

main :: Effect Unit
main = do
  test (empty :: SCComb Int)
  test (pure "hi")
  test digit
  test number
