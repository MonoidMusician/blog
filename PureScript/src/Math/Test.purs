module Math.Test where

import Math.Matrix
import Math.Poly
import Prelude

import Data.Array (range)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Random (random, randomInt)

main :: Effect Unit
main = do
  when true testPoly

-- P = (x-r)Q + R

testPoly :: Effect Unit
testPoly = do
  testSolve $ Poly2 (-4.0) 0.0 2.0
  testSolve $ Poly2 (-4.0) 1.0 4.0
  testSolve $ Poly3 (-4.0) 0.0 2.0 1.0
  testSolve $ Poly3 (-4.0) 1.0 4.0 1.0
  testSolve $ Poly3 4.0 0.0 2.0 1.0 -- -2.59431
  testSolve $ Poly3 4.0 1.0 4.0 1.0
  testSolve $ Poly3 4.0 (-1.0) 4.0 1.0 -- -4.42961
  testSolve $ Poly3 4.0 (-1.0) 4.0 (-1.0)
  testSolve $ Poly3 4.0 (-16.0) 1.0 (-1.0)
  testSolve $ Poly4 4.0 1.0 4.0 1.0 (-1.0)
  testSolve $ Poly4 4.0 1.0 (-4.0) 1.0 (-1.0)
  testSolve $ Poly4 (-4.0) 1.0 4.0 1.0 (-1.0)
  testSolve $ Poly4 (-4.0) 1.0 (-4.0) 1.0 (-1.0)
  testSolve $ Poly4 (-4.0) 1.0 (-4.0) 1.0 1.0

  for_ (range 0 15) \_ -> do
    r1 <- random
    r2 <- random
    r3 <- random
    r4 <- random
    Console.logShow $ [ r1, r2, r3, r4 ]
    testSolve $ unsolve @Poly4 $ Solve44 r1 r2 r3 r4
  for_ (range 0 15) \_ -> do
    r1 <- toNumber <$> randomInt (-123) 123
    r2 <- toNumber <$> randomInt (-123) 123
    r3 <- toNumber <$> randomInt (-123) 123
    r4 <- toNumber <$> randomInt (-123) 123
    Console.logShow $ [ r1, r2, r3, r4 ]
    testSolve $ unsolve @Poly4 $ Solve44 r1 r2 r3 r4
    testSolve $ unsolve @Poly4 $ Solve44 r1 r1 r3 r4
    testSolve $ unsolve @Poly4 $ Solve44 r1 r1 r3 r3

testSolve :: forall poly deriv sol. Solve poly sol => IsPoly poly => Deriv (poly V1) (deriv V1) => IsPoly deriv => Show (poly Number) => poly Number -> Effect Unit
testSolve poly = do
  Console.logShow poly
  let sols = solveN poly <#> \r -> Tuple r $ (wrap <$> poly) @@. r
  case sols of
    EverywhereN -> Console.log "  Everywhere"
    SolveN [] -> Console.log "  Nowhere!"
    SolveN points -> for_ points \(Tuple r val) -> do
      Console.log $ "  - " <> show r <> ", output: " <> show val
      let r' = newtons 5 poly r
      Console.log $ "    " <> show r' <> ", output: " <> show (map wrap poly@@.r')
      let r'' = newtons 15 poly r
      Console.log $ "    " <> show r'' <> ", output: " <> show (map wrap poly@@.r'')
      Console.log $ "    delta: " <> show (r'' - r')
      let r''' = newtons 150 poly r
      Console.log $ "    " <> show r''' <> ", output: " <> show (map wrap poly@@.r''')
      Console.log $ "    delta: " <> show (r''' - r')
