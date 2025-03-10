module Riverdragon.Dragon.Ansi where

import Prelude

import Ansi.Codes as Ansi
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Stylish.Types (Classy(..), Stylish(..))

ansiToAttrs ::
  List (Array Ansi.GraphicsParam) ->
  Array Ansi.GraphicsParam ->
  { classes :: Classy
  , style :: Stylish
  }
ansiToAttrs stack graphics = foldMap convert (preprocess graphics)
  where
  out = mempty :: { classes :: Classy, style :: Stylish }
  ansi_ cls = Classy $ "ansi-" <> cls

  status =
    let
      go List.Nil = { dim: false, fg: Nothing, bg: Nothing }
      go (List.Cons Ansi.Reset _) = { dim: false, fg: Nothing, bg: Nothing }
      go (List.Cons (Ansi.PForeground c) r) = (go r) { fg = Just c }
      go (List.Cons (Ansi.PBackground c) r) = (go r) { bg = Just c }
      go (List.Cons (Ansi.PMode Ansi.Dim) r) = (go r) { dim = true }
      go (List.Cons _ r) = go r
    in go (Array.toUnfoldable =<< List.Cons graphics stack)

  convert = case _ of
    Ansi.Reset -> out { classes = ansi_"reset" }
    Ansi.PMode mode -> case mode of
      Ansi.Bold -> out { classes = ansi_"bold", style = Stylish "font-weight: bold" }
      Ansi.Italic -> out { classes = ansi_"italic", style = Stylish "font-style: italic" }
      Ansi.Underline -> out { classes = ansi_"underline", style = Stylish "text-decoration: underline" }
      Ansi.Strikethrough -> out { classes = ansi_"strikethrough", style = Stylish "text-decoration: line-through" }
      Ansi.Dim -> out { classes = ansi_"dim" } <> case status of
        { fg, bg } ->
          foldMap (convert <<< Ansi.PForeground) fg <>
          foldMap (convert <<< Ansi.PBackground) bg
      Ansi.Inverse -> out { classes = ansi_"inverse" } <> case status of
        { fg, bg } ->
          foldMap (convert <<< Ansi.PForeground) fg <>
          foldMap (convert <<< Ansi.PBackground) bg
    Ansi.PForeground color -> out
      { classes = ansi_$"fg-"<>colorName color
      , style = color_ "color:" color
      }
    Ansi.PBackground color -> out
      { classes = ansi_$"bg-"<>colorName color
      , style = color_ "background-color:" color
      }

  colorName color = (if status.dim then "dim-" else "") <>
    String.toLower
      (String.replace (String.Pattern "Bright") (String.Replacement "bright-") (show color))
  color_ prop clr = Stylish $ prop <> case clr of
    Ansi.Black -> "black"
    Ansi.Red -> "maroon"
    Ansi.Green -> "green"
    Ansi.Yellow -> "gold"
    Ansi.Blue -> "navy"
    Ansi.Magenta -> "purple"
    Ansi.Cyan -> "teal"
    Ansi.White -> "lightgray"
    Ansi.BrightBlack -> "gray"
    Ansi.BrightRed -> "red"
    Ansi.BrightGreen -> "lime"
    Ansi.BrightYellow -> "yellow"
    Ansi.BrightBlue -> "blue"
    Ansi.BrightMagenta -> "magenta"
    Ansi.BrightCyan -> "cyan"
    Ansi.BrightWhite -> "white"

  preprocess = Array.reverse
    >>> Array.takeWhile (_ /= Ansi.Reset)
    >>> Array.nubByEq graphicsConflict
    >>> Array.reverse
  graphicsConflict :: Ansi.GraphicsParam -> Ansi.GraphicsParam -> Boolean
  graphicsConflict = case _, _ of
    Ansi.Reset, Ansi.Reset -> true
    Ansi.PForeground _, Ansi.PForeground _ -> true
    Ansi.PBackground _, Ansi.PBackground _ -> true
    Ansi.PMode a, Ansi.PMode b -> a == b
    _, _ -> false
