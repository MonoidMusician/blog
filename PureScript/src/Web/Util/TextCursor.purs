module Web.Util.TextCursor where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Lens (Lens', Traversal', lens', over, view, wander, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Setter')
import Data.Newtype (class Newtype)
import Data.String (length, null) as S
import Data.String as String
import Data.String.Gen (genUnicodeString)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(..))

-- | Direction ADT. None is the default. See
-- | https://www.w3.org/TR/html5/forms.html#dom-textarea/input-selectiondirection
data Direction = Backward | None | Forward
derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

instance semigroupDirection :: Semigroup Direction where
  append None = identity
  append c = const c
instance monoidDirection :: Monoid Direction where
  mempty = None

instance showDirection :: Show Direction where
  show Forward = "forward"
  show None = "none"
  show Backward = "backward"

-- | The `TextCursor` type represents text selection within an input element.
-- | It consists of three regions of text: the text before the cursor, the text
-- | selected, and the text after the selection. This allows replacements to
-- | occur while keeping intact the cursor position/selection.
newtype TextCursor = TextCursor
  { before :: String
  , selected :: String
  , after :: String
  , direction :: Direction
  }

derive instance textCursorNewtype :: Newtype TextCursor _

instance eqTextCursor :: Eq TextCursor where
  eq (TextCursor l) (TextCursor r) =
    l.before == r.before &&
    l.selected == r.selected &&
    l.after == r.after &&
    r.direction == r.direction

instance showTextCursor :: Show TextCursor where
  show = case _ of
    TextCursor { before: "", selected: "", after: "" } ->
      "«»"
    TextCursor { before, selected: "", after } ->
      "«" <> before <> "|" <> after <> "»"
    TextCursor { before, selected, after } ->
      "«" <> before <> "[|" <> selected <> "|]" <> after <> "»"

genTextCursor :: forall m. MonadRec m => MonadGen m => m TextCursor
genTextCursor =
  mkTextCursor
  <$> genUnicodeString
  <*> genUnicodeString
  <*> genUnicodeString

data ContentTest = Null | Any | Full
type ContentPredicate =
  { before :: ContentTest
  , selected :: ContentTest
  , after :: ContentTest
  }
testContent :: ContentPredicate -> TextCursor -> Boolean
testContent
  { before, selected, after }
  (TextCursor { before: b, selected: s, after: a }) =
    test before b && test selected s && test after a
  where
    test Any _ = true
    test Null t = S.null t
    test Full t = not (S.null t)

-- | Get the current text in the field. (Everything before, inside, and after
-- | the selection.)
content :: TextCursor -> String
content (TextCursor { before, selected, after }) = before <> selected <> after

-- | Get the content length.
-- check: length textcursor == length (content textcursor)
length :: TextCursor -> Int
length (TextCursor { before, selected, after }) =
  S.length before + S.length selected + S.length after

-- | Test whether there is no content.
-- check: null textcursor == null (content textcursor)
null :: TextCursor -> Boolean
null = testContent { before: Null, selected: Null, after: Null }

-- | An empty input field. No selection.
-- check: null empty
empty :: TextCursor
empty = TextCursor { before: "", selected: "", after: "", direction: None }

-- | Create a `TextCursor` from three strings.
mkTextCursor :: String -> String -> String -> TextCursor
mkTextCursor before selected after = TextCursor
  { before
  , selected
  , after
  , direction: None
  }

-- | Apply a `Lens` setting a value to an empty `TextCursor`. When used with
-- | `_before`, `_selected`, or `_after` this will provide a `TextCursor` with
-- | only one non-empty field.
single :: Setter' TextCursor String -> String -> TextCursor
single l v = l .~ v $ empty

-- | Lens for the text before the selection. Empty if the cursor is at the
-- | beginning or the selection starts from the beginning.
_before :: Lens' TextCursor String
_before = _Newtype <<< prop (Proxy :: Proxy "before")

before :: TextCursor -> String
before = view _before

-- | Lens for the text that is selected. Empty if nothing is selected.
_selected :: Lens' TextCursor String
_selected = _Newtype <<< prop (Proxy :: Proxy "selected")

selected :: TextCursor -> String
selected = view _selected

-- | Lens for the text after the selection. Empty if the cursor or selection
-- | reaches the end.
_after :: Lens' TextCursor String
_after = _Newtype <<< prop (Proxy :: Proxy "after")

after :: TextCursor -> String
after = view _after


_start :: Lens' TextCursor Int
_start = lens' \tc -> start tc /\
  \start' ->
    if start' >= end tc then
      placeCursorAt start' tc
    else placeSelection start' (end tc) tc

start :: TextCursor -> Int
start (TextCursor tc) = String.length tc.before

_end :: Lens' TextCursor Int
_end = lens' \tc -> end tc /\
  \end' ->
    if end' <= start tc then
      placeCursorAt end' tc
    else placeSelection (start tc) end' tc

end :: TextCursor -> Int
end (TextCursor tc) = String.length tc.before + String.length tc.selected

-- | Lens for the direction of selection.
_direction :: Lens' TextCursor Direction
_direction = _Newtype <<< prop (Proxy :: Proxy "direction")

-- | Lens for traversing/setting all three fields.
_all :: Traversal' TextCursor String
_all = wander \f (TextCursor { before, selected, after }) ->
  mkTextCursor <$> f before <*> f selected <*> f after

_nonselected :: Traversal' TextCursor String
_nonselected = wander \f (TextCursor { before, selected, after }) ->
  mkTextCursor <$> f before <*> pure selected <*> f after

-- | Test whether the cursor or selection touches the start.
atStart :: TextCursor -> Boolean
atStart = testContent { before: Null, selected: Null, after: Any }

-- | Test whether the cursor or selection reaches the end.
atEnd :: TextCursor -> Boolean
atEnd = testContent { before: Any, selected: Any, after: Null }

-- | Test whether the cursor has selected the whole field.
allSelected :: TextCursor -> Boolean
allSelected = testContent { before: Null, selected: Any, after: Null }

-- | Test whether the selection is collapsed, i.e. acts like a cursor.
isCursor :: TextCursor -> Boolean
isCursor = testContent { before: Any, selected: Null, after: Any }

-- | Test whether the cursor is at the start with no selection.
cursorAtStart :: TextCursor -> Boolean
cursorAtStart = isCursor && atStart

-- | Test whether the cursor is at the end with no selection.
cursorAtEnd :: TextCursor -> Boolean
cursorAtEnd = isCursor && atEnd

-- | Test whether some text is selected.
isSelection :: TextCursor -> Boolean
isSelection = testContent { before: Any, selected: Full, after: Any }

-- | Test whether there is a selection that starts at the beginning.
selectionAtStart :: TextCursor -> Boolean
selectionAtStart = isSelection && atStart

-- | Test whether there is a selection that reaches the end.
selectionAtEnd :: TextCursor -> Boolean
selectionAtEnd = isSelection && atEnd

-- | Select all of the text in a field.
-- |
-- | Note: selection direction is not specified.
-- Content-preserving idempotent endomorphism
selectAll :: TextCursor -> TextCursor
selectAll tc = TextCursor
  { before: ""
  , selected: content tc
  , after: ""
  , direction: tc ^. _direction
  }

-- | Place the cursor to the start of a field, preserving the overall text
-- | content.
-- Content-preserving idempotent endomorphism
placeCursorAtStart :: TextCursor -> TextCursor
placeCursorAtStart tc = TextCursor
  { before: ""
  , selected: ""
  , after: content tc
  , direction: tc ^. _direction
  }

-- | Place the cursor to the end of a field, preserving the overall text
-- | content.
-- Content-preserving idempotent endomorphism
placeCursorAtEnd :: TextCursor -> TextCursor
placeCursorAtEnd tc = TextCursor
  { before: content tc
  , selected: ""
  , after: ""
  , direction: tc ^. _direction
  }

placeCursorAt :: Int -> TextCursor -> TextCursor
placeCursorAt i tc = TextCursor
  { before: split.before
  , selected: ""
  , after: split.after
  , direction: None
  }
  where
  split = String.splitAt i (content tc)

placeSelection :: Int -> Int -> TextCursor -> TextCursor
placeSelection i j tc = TextCursor
  { before: splitL.before
  , selected: splitR.before
  , after: splitR.after
  , direction:
      if i < j then
        tc ^. _direction
      else if i == j then
        None
      else
        Backward
  }
  where
  lower = min i j
  upper = max i j
  splitL = String.splitAt lower (content tc)
  splitR = String.splitAt (upper - lower) splitL.after

-- | Modify just the selected region with an endomorphism.
modifySelected :: (String -> String) -> TextCursor -> TextCursor
modifySelected = over _selected

-- | Modify the non-selected regions with an endomorphism.
modifyNonselected :: (String -> String) -> TextCursor -> TextCursor
modifyNonselected = over _nonselected

-- | Map all three regions of the `TextCursor` with an endomorphism, performing
-- | a replacement or other transformation such as normalization.
modifyAll :: (String -> String) -> TextCursor -> TextCursor
modifyAll = over _all

-- | Prepend a string, on the left.
appendl :: String -> TextCursor -> TextCursor
appendl s tc = over _before (s <> _) tc

-- | Append a string, on the right.
appendr :: TextCursor -> String -> TextCursor
appendr tc s = over _after (_ <> s) tc

-- | Insert a string at the cursor position. If text is selected, the insertion
-- | will be part of the selection. Otherwise it is inserted before the cursor.
-- Preserves normalization
-- check:
--     length (insert insertion textcursor)
--  == length insertion + length textcursor
insert :: String -> TextCursor -> TextCursor
insert insertion = case _ of
  tc@(TextCursor { selected: "" }) ->
    over _before (\before -> before <> insertion) tc
  tc ->
    over _selected (\selected -> selected <> insertion) tc
