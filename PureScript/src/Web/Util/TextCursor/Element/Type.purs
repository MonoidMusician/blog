module Web.Util.TextCursor.Element.Type
  ( TextCursorElement(..)
  , toHTMLElement
  , read, readEventTarget
  , validate, validate'
  , lookupAndValidate
  , lookupValidateAndDo
  ) where

import Control.Alternative ((<|>))
import Control.Monad ((<=<))
import Data.Array (elem)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Prelude (Unit, bind, map, pure, (<$>), (<#>), (>>=))
import Web.DOM.Element (fromEventTarget) as Element
import Web.DOM.ElementId (ElementId)
import Web.DOM.Internal.Types (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event, target)
import Web.HTML (window, HTMLElement, HTMLInputElement, HTMLTextAreaElement)
import Web.HTML.HTMLDocument (toNonElementParentNode) as Document
import Web.HTML.HTMLInputElement (type_)
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.HTMLTextAreaElement as HTextArea
import Web.HTML.Window (document)

-- | A container for the two usable `Element` types:
-- | - `HTMLInputElement`
-- | - `HTMLTextAreaElement`
-- |
-- | Note that not all `HTMLInputElement` nodes are valid, as they must contain
-- | text content. See `validate` for specifics.
-- |
-- | Common operations are defined in `TextCursor.Element.HTML`.
data TextCursorElement = Input HTMLInputElement | TextArea HTMLTextAreaElement

-- | Convert a `TextCursorElement` to a generic `HTMLElement`. Useful for
-- | `focus`.
toHTMLElement :: TextCursorElement -> HTMLElement
toHTMLElement (Input e) = HInput.toHTMLElement e
toHTMLElement (TextArea e) = HTextArea.toHTMLElement e

-- | Read a `TextCursorElement` from an `Element`.
read :: Element -> Maybe TextCursorElement
read e = ta <|> i
  where
    ta = TextArea <$> HTextArea.fromElement e
    i = Input <$> HInput.fromElement e

-- | Read a `TextCursorElement` from the `target` field of an `Event`.
readEventTarget :: Event -> Maybe TextCursorElement
readEventTarget = read <=< Element.fromEventTarget <=< target

-- | Validate a `TextCursorElement`. Input fields need to have one of the
-- | following types when this is called:
-- | - text (default)
-- | - email
-- | - search
-- | - url
validate :: TextCursorElement -> Effect (Maybe TextCursorElement)
validate = case _ of
  tae@(TextArea _e) -> pure (Just tae)
  Input e -> map Input <$> validateInput e
  where
    validateInput :: HTMLInputElement -> Effect (Maybe HTMLInputElement)
    validateInput e = do
      inputtype <- type_ e
      pure if elem inputtype whitelist
        then Just e
        else Nothing
      where
        whitelist = ["", "text", "email", "search", "url"]

-- | Convert from a `Foreign` error computation (type `F`) to a validated
-- | `TextCursorElement`.
validate' :: Maybe TextCursorElement -> Effect (Maybe TextCursorElement)
validate' f =
  case f of
    Nothing -> pure Nothing
    Just e -> validate e

-- | Look up a `TextCursorElement` in the document by id.
lookupAndValidate :: ElementId -> Effect (Maybe TextCursorElement)
lookupAndValidate name = do
  win <- window
  doc <- Document.toNonElementParentNode <$> document win
  getElementById name doc <#> map read >>= maybe (pure Nothing) validate'

-- | Look up a `TextCursorElement` by id and run an action if found.
lookupValidateAndDo :: ElementId -> (TextCursorElement -> Effect Unit) -> Effect Unit
lookupValidateAndDo name action =
  lookupAndValidate name >>= traverse_ action
