module Web.Util.TextCursor.Monoid where

import Prelude
import Web.Util.TextCursor
  ( TextCursor, empty
  , appendl, appendr
  , isCursor, content
  )
import Data.Newtype (class Newtype)

newtype Leftmost = Leftmost TextCursor
newtype Rightmost = Rightmost TextCursor
derive instance leftmostTCNewtype :: Newtype Leftmost _
derive instance rightmostTCNewtype :: Newtype Rightmost _
derive newtype instance showLeftmost :: Show Leftmost
derive newtype instance showRightmost :: Show Rightmost
instance leftmostMonoid :: Monoid Leftmost where mempty = Leftmost empty
instance rightmostMonoid :: Monoid Rightmost where mempty = Rightmost empty

instance leftmostSemigroup :: Semigroup Leftmost where
  append (Leftmost l) (Leftmost r)
    | isCursor l = Leftmost (content l `appendl` r)
    | otherwise  = Leftmost (l `appendr` content r)

instance rightmostSemigroup :: Semigroup Rightmost where
  append (Rightmost l) (Rightmost r)
    | isCursor r = Rightmost (l `appendr` content r)
    | otherwise  = Rightmost (content l `appendl` r)
