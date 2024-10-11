module Web.Util.TextCursor.Element.HTML
  ( value, setValue
  , selectionStart, setSelectionStart
  , selectionEnd, setSelectionEnd
  , selectionDirection, setSelectionDirection
  ) where

import Prelude

import Effect (Effect)
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.HTMLTextAreaElement as HTextArea
import Web.HTML (HTMLInputElement, HTMLTextAreaElement)
import Web.Util.TextCursor (Direction(..))
import Web.Util.TextCursor.Element.Type (TextCursorElement(TextArea, Input))

type Getter a = TextCursorElement -> Effect a
type Setter a = a -> TextCursorElement -> Effect Unit

-- | Lift a pair of getters (Input/TextArea).
getter ::
  forall a.
  (HTMLInputElement -> a) ->
  (HTMLTextAreaElement -> a) ->
  TextCursorElement -> a
getter f _ (Input e) = f e
getter _ g (TextArea e) = g e

-- | Lift a pair of setters (Input/TextArea).
setter ::
  forall a b.
  (b -> HTMLInputElement -> a) ->
  (b -> HTMLTextAreaElement -> a) ->
  b -> TextCursorElement -> a
setter f _ v (Input e) = f v e
setter _ g v (TextArea e) = g v e

-- | Get the current text value of a `TextCursorElement`.
value :: Getter String
value = getter HInput.value HTextArea.value

-- | Set the text value of a `TextCursorElement` to the specified string.
setValue :: Setter String
setValue = setter HInput.setValue HTextArea.setValue

-- | Get the index of the start of the selection.
selectionStart :: Getter Int
selectionStart = getter HInput.selectionStart HTextArea.selectionStart

-- | Set the index of the start of the selection.
setSelectionStart :: Setter Int
setSelectionStart = setter HInput.setSelectionStart HTextArea.setSelectionStart

-- | Get the index of the end of the selection.
selectionEnd :: Getter Int
selectionEnd = getter HInput.selectionEnd HTextArea.selectionEnd

-- | Set the index of the end of the selection.
setSelectionEnd :: Setter Int
setSelectionEnd = setter HInput.setSelectionEnd HTextArea.setSelectionEnd

-- | Get the direction of the selection.
selectionDirection :: Getter Direction
selectionDirection =
  getter HInput.selectionDirection HTextArea.selectionDirection
    >>> map case _ of
      "forward" -> Forward
      "backward" -> Backward
      _ -> None

-- | Set the direction of the selection
setSelectionDirection :: Setter Direction
setSelectionDirection = show >>>
  setter HInput.setSelectionDirection HTextArea.setSelectionDirection
