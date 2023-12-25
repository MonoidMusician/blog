module Parser.Languages.Polish where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldMap, oneOf)
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Idiolect ((/|\), (<#?>))
import Parser.Comb.Comber (Comber, many, manyL, namedRec, rawr, (#:))
import Parser.Comb.Comber as Comber
import Parser.Languages.CSS (test)
import Parser.Types (CST(..))

useWS :: Boolean
useWS = true

trimWS :: String -> String
trimWS = identity
-- trimWS = String.replaceAll (String.Pattern " ") (String.Replacement "")

wss :: Comber Unit
wss | useWS = Comber.wss
wss = pure unit

operators :: Array (String /\ Int)
operators =
  [ "ADD" /\ 2
  , "SUB" /\ 2
  , "MUL" /\ 2
  , "DIV" /\ 2
  , "NEG" /\ 1
  , "NOT" /\ 1
  , "INV" /\ 1
  , "IF" /\ 2
  , "E" /\ 0
  , "ITE" /\ 3
  ]

arities :: Array Int
arities = Array.nub $ map snd operators

arityEq :: Int -> String -> Maybe String
arityEq i s = s <$ (Array.find (eq (s /\ i)) operators)

arity :: String -> Maybe (String /\ Int)
arity s = Array.find (fst >>> eq s) operators

wsrawr :: String
wsrawr | useWS = ""
wsrawr = "(?:\\s+|$)"

op :: Comber String
op = rawr $ "[a-zA-Z]+" <> wsrawr

num :: Comber Number
num = rawr ("\\d+(\\.\\d+)?" <> wsrawr) <#?> Number.fromString

data Strat = Explicit | ManyR | ManyL
-- 16 10 13
strats = ([Explicit,ManyR,ManyL] :: Array Strat)

polish :: Strat -> Comber (CST String Number)
polish strat =
  namedRec "polish" \rec ->
    oneOf
      [ "num"#: Leaf <$> num
      , case strat of
          Explicit ->
            oneOf $ arities <#> \i -> ado
              b <- "op"#: op <#?> arityEq i
              cs <- sequence $ Array.replicate i $ wss *> rec
              in Branch b cs
          ManyR -> ado
            b /\ cs <- ((op <#?> arity) /|\ many "arguments" (wss *> rec)) <#?>
              \((b /\ i) /\ cs) -> b /\ cs <$ guard (Array.length cs == i)
            in Branch b cs
          ManyL -> ado
            b /\ cs <- ((op <#?> arity) /|\ manyL "arguments" (wss *> rec)) <#?>
              \((b /\ i) /\ cs) -> b /\ cs <$ guard (Array.length cs == i)
            in Branch b cs
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
  test (strats <#> \strat -> printPN <$> polish strat) $ map (join bimap trimWS)
    [ Right "123"
    , Left "NOT"
    , Left "MOP"
    , Right "E"
    , Left "E 1"
    , Left "ADD 1"
    , Right "NOT 123"
    , Right "ADD 123 123"
    , Right "ADD NOT 123 123"
    , Right "ADD 123 ADD 123 123"
    , Right "NOT E"
    , Right "ADD 1 E"
    , Right "ADD E 1"
    , Right "ITE 1 2 3"
    , Right "ITE NOT 1 2 3"
    , Left "ITE MUL 1 2 3"
    , Right "ITE MUL 1 2 3 4"
    , Right "ITE 1 2 MUL 3 4"
    , Left "ITE 1 2 3 MUL 4"
    ]

printPN :: CST String Number -> String
printPN (Leaf n) = show n
printPN (Branch b cs) =
  "(" <> b <> foldMap (\c -> " " <> printPN c) cs <> ")"
