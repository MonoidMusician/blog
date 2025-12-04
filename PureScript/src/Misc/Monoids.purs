module Misc.Monoids where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), drop, dropEnd, length, take, takeEnd, zipWith)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck', (===))

type Overlap = List
data CustomOverlap t
  = CustomOverlap
    { overlapFront :: Overlap t
    , inner :: List t
    , overlapRear :: Overlap t
    }
  | JustOverlap (List t)

forget :: forall t. CustomOverlap t -> List t
forget (JustOverlap items) = items
forget (CustomOverlap sections) =
  sections.overlapFront <> sections.inner <> sections.overlapRear

instance Semigroup t => Semigroup (CustomOverlap t) where
  -- We need to handle empty lists explicitly,
  -- otherwise they do not form the identity
  append (JustOverlap Nil) rs = rs
  append ls (JustOverlap Nil) = ls
  append (JustOverlap ls) (JustOverlap rs) =
    case compare (length ls) (length rs) of
      -- If they line up exactly, we can merge them exactly
      EQ -> JustOverlap (zipWith (<>) ls rs)
      -- Otherwise we merge their overlap and the longer one hangs out
      LT -> CustomOverlap
        { overlapFront: zipWith (<>) ls (take (length ls) rs)
        , inner: Nil
        , overlapRear: drop (length ls) rs
        }
      GT -> CustomOverlap
        { overlapFront: dropEnd (length rs) ls
        , inner: Nil
        , overlapRear: zipWith (<>) (takeEnd (length rs) ls) rs
        }
  append (JustOverlap overlapRear) rs =
    append (CustomOverlap { overlapFront: Nil, inner: Nil, overlapRear }) rs
  append ls (JustOverlap overlapFront) =
    append ls (CustomOverlap { overlapFront, inner: Nil, overlapRear: Nil })
  append (CustomOverlap ls) (CustomOverlap rs) =
    CustomOverlap
      { overlapFront: ls.overlapFront
      , inner: ls.inner <> forget (
          JustOverlap ls.overlapRear <> JustOverlap rs.overlapFront
        ) <> rs.inner
      , overlapRear: rs.overlapRear
      }
instance Semigroup t => Monoid (CustomOverlap t) where
  mempty = JustOverlap mempty

instance Arbitrary t => Arbitrary (CustomOverlap t) where
  arbitrary = CustomOverlap <$> arbitrary <|> JustOverlap <$> arbitrary

derive instance Eq t => Eq (CustomOverlap t)
derive instance Generic (CustomOverlap t) _
instance Show t => Show (CustomOverlap t) where show = genericShow

main :: Effect Unit
main = do
  let assoc x y (z :: CustomOverlap (List Int)) = (x <> y) <> z === x <> (y <> z)
  quickCheck' 5000 assoc
