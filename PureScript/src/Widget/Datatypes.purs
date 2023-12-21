module Widget.Datatypes where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=))
import Deku.Control (text_)
import Deku.DOM as D
import Effect.Class.Console (log)
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Element (getAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent)
import Widget (Widget)
import Widget.Types (SafeNut(..))

type DatatypeWidget =
  { widget :: Widget
  -- , parser :: ?help
  }

datatypes :: Map String DatatypeWidget
datatypes = Map.fromFoldable
  [ "abbr" /\
    { widget: \{ text, children } -> do
        abbr <- text
        childs <- children
        case Map.lookup abbr abbreviations of
          Just meaning ->
            pure $ SafeNut do
              text_ meaning
          Nothing -> do
            log $ "Unknown abbrevation " <> show abbr
            pure $ SafeNut mempty
    }
  , "color" /\
    { widget: \{ text, children } -> do
        color <- text
        self <- fold <$> children
        pure $ SafeNut case self of
          SafeNut embed ->
            embed <> text_ " " <> D.span (D.Style !:= ("background-color:"<>color) <|> D.Class !:= "color-sample") []
    }
  ]

abbreviations :: Map String String
abbreviations = Map.fromFoldable
  [ "WIP" /\ "Work In Progress"
  , "TODO" /\ "To Do"
  ]

widget :: Widget
widget interface@{ rawAttr, text } = do
  datatype <- rawAttr (AttrName "data-t") <#> fromMaybe ""
  log datatype
  case Map.lookup datatype datatypes of
    Nothing -> do
      log $ "Unknown datatype " <> show datatype
      pure (SafeNut mempty)
    Just datatypeWidget -> do
      log "Found"
      datatypeWidget.widget interface
