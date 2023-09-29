module Parser.Languages.Polish where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap, oneOf)
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
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
  , "E" /\ 0
  , "ITE" /\ 3
  , "IF" /\ 2
  ]

arities :: Array Int
arities = Array.nub $ map snd operators

arityEq :: Int -> String -> Maybe String
arityEq i s = s <$ (Array.find (eq (s /\ i)) operators)

arity :: String -> Maybe (String /\ Int)
arity s = Array.find (fst >>> eq s) operators

op :: Comber String
op = rawr "[a-zA-Z]+"

num :: Comber Number
num = rawr "\\d+(\\.\\d+)?" <?> Number.fromString

polish :: Comber (CST String Number)
polish =
  namedRec "polish" \rec ->
    oneOf
      [ "num"#: Leaf <$> num
      , oneOf $ arities <#> \i -> ado
          b <- "op"#: op <?> arityEq i
          cs <- sequence $ Array.replicate i (wss *> rec)
          in Branch b cs
      -- , ado
      --   b /\ cs <- ((op <?> arity) /|\ many "arguments" (wss *> rec)) <?>
      --     \((b /\ i) /\ cs) -> b /\ cs <$ guard (Array.length cs == i)
      --   in Branch b cs
      -- , ado
      --     b /\ cs <- ((op <?> arity) /|\ manyL "arguments" (wss *> rec)) <?>
      --       \((b /\ i) /\ cs) -> b /\ cs <$ guard (Array.length cs == i)
      --     in Branch b cs
      ]
    -- namedRec "arguments" \args ->
    --   oneOf
    --     [ Leaf <$> num
    --     , ado
    --         b <- op <?> arityEq 0
    --         in Branch b []
    --     , ado
    --         b <- op <?> arityEq 1
    --         c1 <- wss *> rec
    --         in Branch b [c1]
    --     , ado
    --         b <- op <?> arityEq 2
    --         c1 <- wss *> rec
    --         c2 <- wss *> rec
    --         in Branch b [c1,c2]
    --     ]

main :: Effect Unit
main = do
  test (printPN <$> polish)
    [ "123"
    -- , "NOT"
    -- , "MOP"
    -- , "E"
    -- , "E 1"
    -- , "ADD 1"
    -- , "NOT 123"
    -- , "ADD 123 123"
    , "ADD NOT 123 123"
    -- , "ADD 123 ADD 123 123"
    -- , "NOT E"
    -- , "ADD 1 E"
    -- , "ADD E 1"
    -- , "ITE 1 2 3"
    -- , "ITE NOT 1 2 3"
    -- , "ITE MUL 1 2 3"
    -- , "ITE MUL 1 2 3 4"
    -- , "ITE 1 2 MUL 3 4"
    -- , "ITE 1 2 3 MUL 4"
    ]

printPN :: CST String Number -> String
printPN (Leaf n) = show n
printPN (Branch b cs) =
  "(" <> b <> foldMap (\c -> " " <> printPN c) cs <> ")"
