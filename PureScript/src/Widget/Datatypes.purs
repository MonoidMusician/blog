module Widget.Datatypes where

import Prelude

import Control.Alternative (guard)
import Control.Plus (empty)
import Data.Array (fold)
import Data.Array as Array
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (power)
import Data.Monoid as M
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class.Console (log)
import Riverdragon.Dragon.Bones (Dragon)
import Riverdragon.Dragon.Bones as D
import Riverdragon.Test (host)
import Web.DOM.AttrName (AttrName(..))
import Widget (Widget)
import Widget.Types (SafeNut(..))

type DatatypeWidget =
  { widget :: Widget
  , recognize :: String -> Boolean
  -- , parser :: ?help
  }

linkify :: Boolean -> Maybe String -> Dragon -> Dragon
linkify _ Nothing = identity
linkify external (Just href) =
  D.aN href [ maybe empty (D.attr "target") ("_blank" <$ guard external) ]

nobreak :: String -> String
nobreak = String.replaceAll (String.Pattern " ") (String.Replacement "\xA0")

unabbreviate :: String -> Maybe Abbreviation
unabbreviate abbr =
  oneOf
    [ Map.lookup abbr abbreviations
    , Map.lookup (String.toLower abbr) abbreviations
    , Map.lookup (String.toUpper abbr) abbreviations
    ]

datatypes :: Map String DatatypeWidget
datatypes = Map.fromFoldable
  [ "abbr" /\
    { recognize: isJust <<< unabbreviate
    , widget: \{ text, content } -> host do
        abbr <- text
        case unabbreviate abbr of
          Just meaning -> do
            -- FIXME
            embed <- mempty content
            pure case meaning of
              Acronym expanded ->
                D.span' [ D.className "tooltipped abbreviated" ] $ D.Fragment
                  [ D.span' [ D.className "abbreviation", D.aria_"hidden" "true" ] embed
                  , D.span' [ D.className "tooltip abbreviation" ] $ D.text $ nobreak expanded
                  ]
              Foreign lang value link meanings ->
                D.span' [ D.className "tooltipped abbreviated" ] $ D.Fragment
                  [ D.span' [ D.className "foreign", D.attr "lang" lang ] embed
                  , D.span' [ D.className "tooltip abbreviation", D.aria_"hidden" "true" ] $ D.Fragment
                    [ linkify true link $
                        D.span' [ D.className "foreign", D.attr "lang" lang ] $ D.text $ nobreak value
                    , D.text $ Array.intercalate "," $ meanings <#>
                        \m -> " “" <> nobreak m <> "”"
                    ]
                  ]
          Nothing -> do
            log $ "Unknown abbrevation " <> show abbr
            pure mempty
    }
  , "color" /\
    { recognize: const false
    , widget: \{ text, content } -> host do
        color <- text
        -- FIXME
        embed <- mempty content
        pure
          let
            attrs =
              [ D.className "color-sample sample-after sample"
              , D.style ("background-color:" <> color)
              ]
          in embed <> D.span' attrs mempty
    }
  , "unicode" /\
    { recognize: isJust <<< String.stripPrefix (String.Pattern "U+")
    , widget: \{ text, content, rawAttr } -> host do
        textValue <- text
        case parseUnicode textValue of
          Nothing -> do
            log $ "Invalid codepoint " <> show textValue
            pure mempty
          Just cp -> do
            hide <- maybe false (String.contains (String.Pattern "no-preview")) <$> rawAttr (AttrName "data-c")
            _self <- content
            pure
              let
                attrs =
                  [ D.className "unicode-sample sample-after sample"
                  ]
              in fold
                [ D.span $ D.Fragment
                  [ D.span' [ D.className "meta-code" ] $ D.text "U+"
                  , D.span' [ D.className "code numeric" ] $ D.text $ printUnicodeHex cp
                  ]
                , M.guard (not hide) $ D.span' attrs $ D.text $ String.singleton cp
                ]
    }
  ]

parseUnicode :: String -> Maybe CodePoint
parseUnicode = String.trim >>> \s -> do
  s' <- oneOf
    [ String.stripPrefix (String.Pattern "U+") s
    , String.stripPrefix (String.Pattern "U") s
    , pure s
    ]
  i <- Int.fromStringAs hexadecimal (String.trim s')
  toEnum i

printUnicodeHex :: CodePoint -> String
printUnicodeHex cp = do
  let s = String.toUpper (Int.toStringAs hexadecimal (fromEnum cp))
  case String.length s of
    5 -> "0" <> s
    6 -> s
    l -> power "0" (4 - l) <> s

data Abbreviation
  = Acronym String
  | Foreign String String (Maybe String) (Array String)

abbreviations :: Map String Abbreviation
abbreviations = Map.fromFoldable
  [ "WIP" /\ Acronym "Work In Progress"
  , "TODO" /\ Acronym "To Do"
  , "e.g." /\ Foreign "la" "exempli\x0304 gra\x0304tia\x0304"
      (Just "https://en.wiktionary.org/wiki/e.g.#English")
      ["for example"]
  , "i.e." /\ Foreign "la" "id est"
      (Just "https://en.wiktionary.org/wiki/i.e.#English")
      ["that is"]
  , "cf." /\ Foreign "la" "co\x0304nfer"
      (Just "https://en.wiktionary.org/wiki/cf.#English")
      ["see also", "compare with"]
  , "viz." /\ Foreign "la" "vide\x0304licet"
      (Just "https://en.wiktionary.org/wiki/viz.#English")
      ["namely", "specifically"]
  , "sc." /\ Foreign "la" "sci\x0304licet"
      (Just "https://en.wiktionary.org/wiki/sc.#English")
      ["namely", "as follows"]
  , "tl;dr" /\ Acronym "too long; didnʼt read"
  , "etc." /\ Foreign "la" "et ce\x0304tera"
      (Just "https://en.wiktionary.org/wiki/etc.#English")
      ["and so on", "and the remaining items"]
  , "et al." /\ Foreign "la" "et aliae, et alii\x0304"
      (Just "https://en.wiktionary.org/wiki/et_al.#Etymology_1")
      ["and others"]
  , "NbE" /\ Acronym "Normalization by Evaluation"
  , "GC" /\ Acronym "Garbage Collector"
  , "GCable" /\ Acronym "Garbage Collectable"
  , "N-ary" /\ Acronym "Arbitrary arity, in the sense of unary or binary operators"
  ]

matching :: String -> Array String
matching t =
  Map.toUnfoldable datatypes
    # Array.mapMaybe \(Tuple k { recognize }) ->
      if recognize t then Just k else Nothing

widget :: Widget
widget interface@{ text, rawAttr } = do
  givenDatatype <- rawAttr (AttrName "data-t") <#> fromMaybe ""
  t <- text
  datatype <- pure case givenDatatype of
    "" | [obvious] <- matching t -> obvious
    _ -> givenDatatype
  case Map.lookup datatype datatypes of
    Nothing -> do
      log $ "Unknown datatype " <> show datatype <> " (" <> show t <> ")"
      pure (SafeNut mempty)
    Just datatypeWidget -> do
      datatypeWidget.widget interface
