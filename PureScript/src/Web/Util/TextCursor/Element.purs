module Web.Util.TextCursor.Element
  ( module Web.Util.TextCursor.Element.Type
  , module Web.Util.TextCursor.Element.HTML
  , textCursor, setTextCursor
  , modifyTextCursor, modifyTextCursorST, modifyTextCursorST_
  , focusTextCursor, focusTextCursorById
  ) where

import Prelude

import Control.Monad.State.Class (class MonadState, modify)
import Data.Lens (Lens', (.~), (^.))
import Data.String.CodeUnits (splitAt) as CU
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM.ElementId (ElementId)
import Web.HTML.HTMLElement (focus)
import Web.Util.TextCursor (TextCursor(..), _direction, content, end, start)
import Web.Util.TextCursor.Element.HTML (value, setValue, selectionStart, setSelectionStart, selectionEnd, setSelectionEnd, selectionDirection, setSelectionDirection)
import Web.Util.TextCursor.Element.Type (TextCursorElement(..), toHTMLElement, read, readEventTarget, validate, validate', lookupAndValidate, lookupValidateAndDo)

-- | Get the `TextCursor` from a `TextCursorElement`.
textCursor :: TextCursorElement -> Effect TextCursor
textCursor element = do
  val <- value element
  start <- selectionStart element
  end <- selectionEnd element
  direction <- selectionDirection element
  let { before: prior, after } = CU.splitAt end val
  let { before, after: selected } = CU.splitAt start prior
  pure $ TextCursor
    { before
    , selected
    , after
    , direction
    }

-- | Set the `TextCursor` on a `TextCursorElement`. Calls `setValue`,
-- | `setSelectionStart`, `setSelectionEnd`, and `setSelectionDirection` to
-- | ensure a consistent state for the field.
setTextCursor :: TextCursor -> TextCursorElement -> Effect Unit
setTextCursor tc element = do
  setValue (content tc) element
  setSelectionStart (start tc) element
  setSelectionEnd (end tc) element
  setSelectionDirection (tc ^. _direction) element

-- | Modifies the `TextCursor` on an element through the given endomorphism.
modifyTextCursor :: (TextCursor -> TextCursor) -> TextCursorElement -> Effect Unit
modifyTextCursor f element = do
  tc <- f <$> textCursor element
  setTextCursor tc element

-- | Modifies the `TextCursor` on an element as well as setting the result in a
-- | State+Eff monad via a lens. Useful for components processing input events!
modifyTextCursorST :: forall m s.
  MonadState s m =>
  MonadEffect m =>
  Lens' s TextCursor ->
  (TextCursor -> TextCursor) ->
  TextCursorElement -> m s
modifyTextCursorST l f element = do
  tc <- liftEffect $ f <$> textCursor element
  liftEffect $ setTextCursor tc element
  modify $ l .~ tc

modifyTextCursorST_ :: forall m s.
  MonadState s m =>
  MonadEffect m =>
  Lens' s TextCursor ->
  (TextCursor -> TextCursor) ->
  TextCursorElement -> m Unit
modifyTextCursorST_ l f element = void $ modifyTextCursorST l f element

-- | Focuses an element after setting the `TextCursor`.
focusTextCursor :: TextCursor -> TextCursorElement -> Effect Unit
focusTextCursor tc element = do
  setTextCursor tc element
  focus (toHTMLElement element)

-- | Looks up an element by id to focus with a `TextCursor`.
focusTextCursorById :: ElementId -> TextCursor -> Effect Unit
focusTextCursorById name tc = do
  lookupValidateAndDo name (focusTextCursor tc)
