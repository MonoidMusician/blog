module Riverdragon.Dragon where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), maybe)
import Data.Pair (Pair(..))
import Data.Semigroup.First (First(..))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup.Last (Last(..))
import Data.Traversable (foldMap, for, for_, sequence_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (runEffectFn3)
import Prim.Boolean (False)
import Riverdragon.Dragon.Breath (removeSelf, unsafeSetProperty)
import Riverdragon.River (Stream, subscribe)
import Riverdragon.River.Bed (accumulator, eventListener, globalId, ordMap, rolling)
import Web.DOM (Comment, Node, Element)
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Comment as Comment
import Web.DOM.Document (createComment, createElement, createElementNS, createTextNode)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.ElementName (ElementName(..))
import Web.DOM.NamespaceURI (NamespaceURI(..))
import Web.DOM.Node (appendChild, insertBefore, parentNode, setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Text as Text
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web
import Web.Event.EventPhase (EventPhase(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

data Dragon
  = Fragment (Array Dragon)
  -- Only renders one `Dragon` at once (which may be a `Fragment`, `Collection`,
  -- or whatever)
  | Replaceable (Stream False Dragon)
  -- Appends a new `Dragon` each time
  | Collection (Stream False Dragon)
  | Element (Maybe String) String (Stream False AttrProp) Dragon
  | Text (Stream False String)

instance semigroupDragon :: Semigroup Dragon where
  append (Fragment []) r = r
  append l (Fragment []) = l
  append (Fragment l) (Fragment r) = Fragment (l <> r)
  append l r = Fragment [l,r]
instance monoidDragon :: Monoid Dragon where
  mempty = Fragment []

data AttrProp
  = Attr (Maybe String) String String
  | Prop String PropVal
  | Listener String (Maybe (Web.Event -> Effect Unit))

data PropVal
  = PropString String
  | PropBoolean Boolean
  | PropInt Int
  | PropNumber Number

renderProps :: Element -> Stream False AttrProp -> Effect (Effect Unit)
renderProps el stream = do
  listeners <- ordMap
  let
    receive = case _ of
      Attr ns name val -> setAttribute (AttrName name) val el
      Prop name valTy -> case valTy of
        PropString val -> runEffectFn3 unsafeSetProperty el name val
        PropBoolean val -> runEffectFn3 unsafeSetProperty el name val
        PropInt val -> runEffectFn3 unsafeSetProperty el name val
        PropNumber val -> runEffectFn3 unsafeSetProperty el name val
      Listener ty mcb -> do
        (listeners.remove ty) >>= sequence_
        case mcb of
          Nothing -> pure unit
          Just cb -> do
            let
              doIt = eventListener
                { eventPhase: Bubbling
                , eventTarget: Element.toEventTarget el
                , eventType: EventType ty
                }
            r <- doIt cb
            _ <- listeners.set ty r
            pure unit
  unsubscribe <- subscribe stream receive
  pure do
    unsubscribe
    listeners.reset >>= traverse_ identity

mkBookmarks :: (Node -> Effect Unit) -> Effect (Tuple (Pair Comment) (Node -> Effect Unit))
mkBookmarks insert = do
  doc <- window >>= document
  bookmarks@(Pair _ bookend) <- for (Pair unit unit) \_ -> do
    this <- globalId -- for hydration?
    bookmark <- createComment (show this) (HTMLDocument.toDocument doc)
    insert (Comment.toNode bookmark)
    pure bookmark
  pure $ Tuple bookmarks \newSibling -> do
    parentNode (Comment.toNode bookend) >>= traverse_ (insertBefore newSibling (Comment.toNode bookend))

type Rendered =
  { head :: First Node
  , destroy :: Effect Unit
  , tail :: Last Node
  }
only :: Node -> Effect Unit -> Rendered
only node destroy =
  { head: First node
  , destroy
  , tail: Last node
  }
fromBookmarks :: Pair Comment -> Effect Unit -> Rendered
fromBookmarks (Pair l r) destroy =
  { head: First (Comment.toNode l)
  , destroy
  , tail: Last (Comment.toNode r)
  }

renderId :: ElementId -> Dragon -> Effect (Effect Unit)
renderId id dragon = do
  mel <- getElementById id <<< HTMLDocument.toNonElementParentNode =<< document =<< window
  mel # foldMap \el -> do
    Tuple _ insert <- mkBookmarks (appendChild <@> Element.toNode el)
    _.destroy <$> renderTo insert dragon

renderTo :: (Node -> Effect Unit) -> Dragon -> Effect Rendered
renderTo insert (Text changingValue) = do
  doc <- window >>= document
  el <- createTextNode "" (HTMLDocument.toDocument doc)
  unsubscribe <- subscribe changingValue \newValue -> do
    setTextContent newValue do Text.toNode el
  insert do Text.toNode el
  pure $ only (Text.toNode el) do
    unsubscribe
    removeSelf do Text.toNode el
renderTo insert (Element ns ty props children) = do
  doc <- window >>= document
  let create = maybe createElement (createElementNS <<< Just <<< NamespaceURI)
  el <- create ns (ElementName ty) (HTMLDocument.toDocument doc)
  unSubscribeProps <- renderProps el props
  destroyChildren <- renderTo (appendChild <@> Element.toNode el) children
  insert do Element.toNode el
  pure $ only (Element.toNode el) do
    unSubscribeProps
    destroyChildren.destroy
    removeSelf do Element.toNode el
renderTo insert (Fragment children) =
  case NEA.fromArray children of
    Nothing -> do
      doc <- window >>= document
      this <- globalId -- for hydration?
      bookmark <- createComment (show this) (HTMLDocument.toDocument doc)
      pure $ (only <*> removeSelf) (Comment.toNode bookmark)
    Just children' -> foldMap1 (renderTo insert) children'
renderTo insert (Replaceable revolving) = do
  Tuple bookmarks insert' <- mkBookmarks insert
  unsubPrev <- rolling
  unsubscribe <- subscribe revolving \newValue -> do
      unsubPrev mempty
      unsubPrev <<< _.destroy =<< renderTo insert' newValue
  pure $ fromBookmarks bookmarks do
    unsubscribe
    unsubPrev mempty
    for_ bookmarks (Comment.toNode >>> removeSelf)
renderTo insert (Collection items) = do
  Tuple bookmarks insert' <- mkBookmarks insert
  unsubAll <- accumulator
  unsubscribe <- subscribe items \newChild -> do
    unsubAll.put <<< _.destroy =<< renderTo insert' newChild
  pure $ fromBookmarks bookmarks do
    unsubscribe
    join (unsubAll.reset)
    for_ bookmarks (Comment.toNode >>> removeSelf)
