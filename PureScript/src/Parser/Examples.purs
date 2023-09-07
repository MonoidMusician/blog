module Parser.Examples where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (fromFoldable, nub, toUnfoldable)
import Data.Either (Either(..))
import Data.Foldable (intercalate, oneOf, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.String (CodePoint)
import Data.String as CP
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parser.Comb (Comb(..), Combs, codePoint, named, namedRec, parseString, printSyntax, string)
import Parser.Types (Fragment, Grammar(..), Part(..))

mainName :: String
mainName = "main"

mk :: forall a. Combs a -> (String -> Maybe a)
mk = parseString mainName

printBNF :: forall a. Combs a -> Effect Unit
printBNF (Comb { grammar: MkGrammar grammar }) = nub grammar # traverse_ \rule ->
  log $ rule.pName <> "." <> show rule.rName <> " = " <> showFragment rule.rule <> " ."

printPretty :: forall a. Combs a -> Effect Unit
printPretty (Comb { prettyGrammar }) = nub prettyGrammar #
  traverse_ \(name /\ msyntax) ->
    msyntax # traverse_ \syntax ->
      log $ name <> " = " <> printSyntax showFragment syntax <> " ."

showFragment :: Fragment String CodePoint -> String
showFragment =
  map (map CP.singleton) >>> toUnfoldable >>> coalesce >>> fromFoldable >>>
    map showPart >>> intercalate " "

showPart :: Part String String -> String
showPart (Terminal tok) = "\"" <> tok <> "\""
showPart (NonTerminal nt) = nt

coalesce :: List (Part String String) -> List (Part String String)
coalesce (Terminal x : Terminal y : zs) = coalesce (Terminal (x <> y) : zs)
coalesce (z : zs) = z : coalesce zs
coalesce Nil = Nil

digit :: Combs Int
digit = named "digit" $ oneOf $ mapWithIndex (<$) $
  codePoint <$> String.toCodePointArray "0123456789"

number :: Combs Int
number = namedRec "number" \numberRec ->
  digit <|> ado
    ds <- numberRec
    d <- digit
    in 10*ds + d

testData :: Array String
testData =
  [ ""
  , "6"
  , "7"
  , "80"
  , "09"
  , "1234"
  ]

parseNumber :: String -> Maybe Int
parseNumber = mk number

test :: forall a. Show a => Combs a -> Effect Unit
test parser = do
  log ""
  log "Grammar:"
  printPretty (named mainName parser)
  let doParse = mk parser
  log ""
  log "Examples:"
  traverse_ (\s -> log (show s <> " --> " <> show (doParse s))) testData

main :: Effect Unit
main = do
  test (empty :: Combs Int)
  test (pure "hi")
  test digit
  test number
  test (Left <$> digit <|> (map Right <<< (/\)) <$> digit <*> digit)
  test (Left <$> digit <|> Right <$> number)
  test (Right <$> number <|> Left <$> digit)

  log ""
  printPretty $ named mainName $
    (string "abc" <|> string "123") *> (string "xyz" <|> string "789")
