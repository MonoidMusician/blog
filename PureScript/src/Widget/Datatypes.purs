module Widget.Datatypes where

import Prelude

import Control.Plus (empty)
import Data.Array (fold)
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=))
import Deku.Control (text_)
import Deku.DOM as D
import Effect.Class.Console (log)
import Web.DOM.AttrName (AttrName(..))
import Widget (Widget)
import Widget.Types (SafeNut(..))

type DatatypeWidget =
  { widget :: Widget
  -- , parser :: ?help
  }

datatypes :: Map String DatatypeWidget
datatypes = Map.fromFoldable
  [ "abbr" /\
    { widget: \{ text, content } -> do
        abbr <- text
        case Map.lookup abbr abbreviations of
          Just meaning -> do
            _self <- content
            pure $ SafeNut do
              text_ meaning
          Nothing -> do
            log $ "Unknown abbrevation " <> show abbr
            pure $ SafeNut mempty
    }
  , "color" /\
    { widget: \{ text, content } -> do
        color <- text
        self <- content
        pure $ SafeNut
          let
            SafeNut embed = self
            attrs = oneOf
              [ D.Class !:= "color-sample sample-after sample"
              , D.Style !:= ("background-color:" <> color)
              ]
          in embed <> D.span attrs []
    }
  , "unicode" /\
    { widget: \{ text, content } -> do
        textValue <- text
        case parseUnicode textValue of
          Nothing -> do
            log $ "Invalid codepoint " <> show textValue
            pure mempty
          Just cp -> do
            _self <- content
            pure $ SafeNut
              let
                attrs = oneOf
                  [ D.Class !:= "unicode-sample sample-after sample"
                  ]
              in fold
                [ D.code empty
                  [ D.span (D.Class !:= "kw") [text_ "U+"]
                  , D.span (D.Class !:= "dv") [text_ (printUnicodeHex cp)]
                  ]
                , D.span attrs [text_ (String.singleton cp)]
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
  let s = Int.toStringAs hexadecimal (fromEnum cp)
  case String.length s of
    5 -> "0" <> s
    6 -> s
    l -> power "0" (4 - l) <> s

abbreviations :: Map String String
abbreviations = Map.fromFoldable
  [ "WIP" /\ "Work In Progress"
  , "TODO" /\ "To Do"
  ]

widget :: Widget
widget interface@{ rawAttr } = do
  datatype <- rawAttr (AttrName "data-t") <#> fromMaybe ""
  log datatype
  case Map.lookup datatype datatypes of
    Nothing -> do
      log $ "Unknown datatype " <> show datatype
      pure (SafeNut mempty)
    Just datatypeWidget -> do
      log "Found"
      datatypeWidget.widget interface
