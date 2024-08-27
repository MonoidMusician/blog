module Parser.Random where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (for, sequence)
import Effect (Effect)
import Idiolect ((>==))
import Parser.Algorithms (chooseBySize, countTs)
import Parser.Types (Fragment, Part(..), Producible, SProducible, Produced, getRulesFor)
import Random.LCG as LCG
import Test.QuickCheck.Gen as QC

shrink :: forall m. MonadGen m => Int -> m ~> m
shrink n = Gen.resize $ (_ - n) >>> (_ `div` 2)

genNT
  :: forall m meta nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced meta nt r tok)
  -> (nt -> Maybe (m (Array tok)))
genNT grammar nt = genNT1 grammar nt <#> \mr ->
  mr >>= \r -> shrink (countTs r.rule) $ genMore grammar r

genMore
  :: forall m meta nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced meta nt r tok)
  -> { rule :: Fragment meta nt tok, produced :: Array tok }
  -> m (Array tok)
genMore grammar { rule, produced } =
  -- `genMoreMaybe` should never fail, if the `Produced` data is any good
  -- but just in case! we still need to produce a value, so `produced` is a
  -- default value we have access to from the `Produced` data.
  fromMaybe (pure produced) (genMoreMaybe grammar rule)

genMoreMaybe
  :: forall m meta nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced meta nt r tok)
  -> Fragment meta nt tok
  -> Maybe (m (Array tok))
genMoreMaybe grammar rule =
  map (map join <<< sequence) $
    for rule case _ of
      Terminal tok -> Just (pure [ tok ] :: m (Array tok))
      NonTerminal nt -> genNT grammar nt
      InterTerminal _ -> Just (pure [])

genNT1
  :: forall m meta nt r tok
   . Eq nt
  => MonadGen m
  => Array (Produced meta nt r tok)
  -> (nt -> Maybe (m { rule :: Fragment meta nt tok, produced :: Array tok }))
genNT1 grammar nt =
  getRulesFor grammar nt # NEA.fromArray #
    map (\rules -> Gen.sized \sz -> chooseBySize sz rules # Gen.elements)



sample :: forall meta nt r tok. Eq nt => Eq r => Eq tok => Producible meta nt r tok -> Maybe (Array tok)
sample grammar =
  QC.evalGen <$> genNT grammar.produced grammar.grammar.entry <@>
    { size: 15, newSeed: LCG.mkSeed 12345678 }

sampleS :: SProducible -> String
sampleS = sample >>> maybe "" String.fromCodePointArray

sampleE :: forall meta nt r tok. Eq nt => Eq r => Eq tok => Producible meta nt r tok -> Effect (Maybe (Array tok))
sampleE grammar = sequence $
  QC.randomSampleOne <$> genNT grammar.produced grammar.grammar.entry

sampleSE :: SProducible -> Effect String
sampleSE = sampleE >== maybe "" String.fromCodePointArray
