module Parser.Languages.Polish where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (foldMap, oneOf)
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect (Effect)
import Parser.Comb (namedRec)
import Parser.Comb.Types ((<?>))
import Parser.Languages (Comber, rawr, wss, (#:))
import Parser.Languages.CSS (test)
import Parser.Types (CST(..))

operators :: Array (String /\ Int)
operators =
  [ "ADD" /\ 2
  , "SUB" /\ 2
  , "MUL" /\ 2
  , "DIV" /\ 2
  , "NEG" /\ 1
  , "NOT" /\ 1
  , "INV" /\ 1
  ]

arity :: Int -> String -> Maybe String
arity i s = s <$ (Array.find (eq (s /\ i)) operators)

op :: Comber String
op = rawr "[a-zA-Z]+"

num :: Comber Number
num = rawr "\\d+(\\.\\d+)?" <?> Number.fromString

polish :: Comber (CST String Number)
polish = namedRec "polish" \rec ->
  oneOf
    [ Leaf <$> num
    , "unary"#: ado
        b <- op <?> arity 1
        wss
        c1 <- rec
        in Branch b [c1]
    , "binary"#: ado
        b <- op <?> arity 2
        wss
        c1 <- rec
        wss
        c2 <- rec
        in Branch b [c1,c2]
    ]

main :: Effect Unit
main = do
  test (printPN <$> polish)
    [ "123"
    , "NOT"
    , "MOP"
    , "NOT 123"
    , "ADD 123 123"
    , "ADD 123 ADD 123 123"
    ]

printPN :: CST String Number -> String
printPN (Leaf n) = show n
printPN (Branch b cs) =
  "(" <> b <> foldMap (\c -> " " <> printPN c) cs <> ")"
