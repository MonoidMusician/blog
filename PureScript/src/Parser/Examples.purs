module Parser.Examples where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable, nub, toUnfoldable)
import Data.Either (Either(..), choose)
import Data.Foldable (intercalate, oneOf, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Parser.Comb (Comb(..), named, namedRec, parse, parseRegex, printSyntax, token, tokenRawr, tokenStr)
import Parser.Lexing (class ToString, class Token, type (~), Rawr(..), Similar(..), rawr, recognize, toString)
import Parser.Types (Fragment, Grammar(..), Part(..))

type Combs = Comb String CodePoint CodePoint
type Comber = Comb String (String ~ Rawr) String

mainName :: String
mainName = "main"

printBNF :: forall a. Combs a -> Effect Unit
printBNF (Comb { grammar: MkGrammar grammar }) = nub grammar # traverse_ \rule ->
  log $ rule.pName <> "." <> show rule.rName <> " = " <> showFragment rule.rule <> " ."

printPretty :: forall s o a. Ord s => ToString s => Comb String s o a -> Effect Unit
printPretty (Comb { prettyGrammar }) = nub prettyGrammar #
  traverse_ \(name /\ msyntax) ->
    msyntax # traverse_ \syntax ->
      log $ name <> " = " <> printSyntax showFragment syntax <> " ."

printPrettyR :: forall o a. Comb String (String ~ Rawr) o a -> Effect Unit
printPrettyR (Comb { prettyGrammar }) = nub prettyGrammar #
  traverse_ \(name /\ msyntax) ->
    msyntax # traverse_ \syntax ->
      log $ name <> " = " <> printSyntax showFragmentR syntax <> " ."

showFragment :: forall s. ToString s => Fragment String s -> String
showFragment =
  map (map (toString >>> Left >>> Similar)) >>> toUnfoldable >>> coalesce >>> fromFoldable >>>
    map showPart >>> intercalate " "

showFragmentR :: Fragment String (String ~ Rawr) -> String
showFragmentR =
  toUnfoldable >>> coalesce >>> fromFoldable >>>
    map showPart >>> intercalate " "

showPart :: Part String (String ~ Rawr) -> String
showPart (Terminal (Similar (Left tok))) = "\"" <> tok <> "\""
showPart (Terminal (Similar (Right (Rawr re)))) = show re
showPart (NonTerminal nt) = nt

coalesce :: List (Part String (String ~ Rawr)) -> List (Part String (String ~ Rawr))
coalesce (Terminal (Similar (Left x)) : Terminal (Similar (Left y)) : zs) = coalesce (Terminal (Similar (Left (x <> y))) : zs)
coalesce (z : zs) = z : coalesce zs
coalesce Nil = Nil

digit :: Combs Int
digit = named "digit" $ oneOf $ mapWithIndex (<$) $
  token <$> String.toCodePointArray "0123456789"

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

test :: forall cat o a. Show a => Ord cat => ToString cat => Token cat String o => Comb String cat o a -> Effect Unit
test parser = do
  log ""
  log "Grammar:"
  printPretty (named mainName parser)
  let doParse = parse mainName parser
  log ""
  log "Examples:"
  traverse_ (\s -> pure unit >>= \_ -> log (show s <> " --> " <> result (doParse s))) testData

testR :: forall a. Show a => Comber a -> Effect Unit
testR parser = do
  log ""
  log "Grammar:"
  printPrettyR (named mainName parser)
  let doParse = parseRegex mainName parser
  log ""
  log "Examples:"
  traverse_ (\s -> pure unit >>= \_ -> log (show s <> " --> " <> result (doParse s))) testData

result :: forall a. Show a => Either String a -> String
result (Left e) = "✗ " <> e
result (Right r) = "✓ " <> show r

infixr 5 choose as \|/

main :: Effect Unit
main = do
  log $ show $ recognize (rawr "\\w+") "asdf1234.asdf"
  testR do
    tokenRawr "\\d+" \|/ tokenStr "09"
  -- test (empty :: Combs Int)
  -- test (pure "hi" :: Combs String)
  -- test digit
  -- test number
  -- test (Left <$> digit <|> (map Right <<< (/\)) <$> digit <*> digit)
  -- test (Left <$> digit <|> Right <$> number)
  -- test (Right <$> number <|> Left <$> digit)

  -- log ""
  -- printPretty $ named mainName $
  --   (token "abc" <|> token "123") *> (token "xyz" <|> token "789")
