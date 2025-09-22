module Riverdragon.Dragon where

import Prelude

import Control.Monad.ResourceT (ResourceM, start, start_)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Pair (Pair(..))
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.Traversable (for, for_, sequence_, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Console as Console
import Effect.Uncurried (EffectFn3, mkEffectFn3, runEffectFn3)
import Idiolect ((>==))
import Riverdragon.Dragon.Breath (removeSelf, setAttributeNS, unsafeSetProperty)
import Riverdragon.River (Lake, Stream, bursting, subscribe, subscribeIsh)
import Riverdragon.River.Bed (Allocar, accumulator, eventListener, freshId, ordMap, prealloc, pushArray, rolling, unsafeAllocate)
import Riverdragon.River.Beyond (mkAnimFrameBuffer)
import Safe.Coerce (coerce)
import Web.DOM (Comment, Document, Element, Node)
import Web.DOM (Element) as Web
import Web.DOM.Attr (getValue)
import Web.DOM.Attr as Attr
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Comment as Comment
import Web.DOM.Document (createComment, createElement, createElementNS, createTextNode)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.ElementId (ElementId)
import Web.DOM.ElementName (ElementName(..))
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.NamedNodeMap (getAttributes)
import Web.DOM.NamespaceURI (NamespaceURI(..))
import Web.DOM.Node (appendChild, insertBefore, parentNode, setTextContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (children)
import Web.DOM.Text as Text
import Web.Event.Event (Event) as Web
import Web.Event.Event (EventType(..))
import Web.Event.EventPhase (EventPhase(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

data Dragon
  = Fragment (Array Dragon)
  | Egg (ResourceM Dragon)
  -- Only renders one `Dragon` at once (which may be a `Fragment`, `Appending`,
  -- or whatever)
  | Replacing (Lake Dragon)
  -- Appends a new `Dragon` each time
  | Appending (Lake Dragon)
  | Element (Maybe String) String (Lake AttrProp) Dragon
  | Text (Lake String)

instance semigroupDragon :: Semigroup Dragon where
  append (Fragment []) r = r
  append l (Fragment []) = l
  append (Fragment l) (Fragment r) = Fragment (l <> r)
  append (Fragment l) r = Fragment (l <> [r])
  append l (Fragment r) = Fragment ([l] <> r)
  append l r = Fragment [l,r]
instance monoidDragon :: Monoid Dragon where
  mempty = Fragment []

data AttrProp
  = Attr (Maybe String) String String
  | Prop String PropVal
  | Listener String (Maybe (Web.Event -> Effect Unit))
  | MultiAttr (Array AttrProp)
  | Self (Element -> Effect (Effect Unit))
instance semigroupAttrProp :: Semigroup AttrProp where
  append (MultiAttr []) r = r
  append l (MultiAttr []) = l
  append (MultiAttr l) (MultiAttr r) = MultiAttr (l <> r)
  append (MultiAttr l) r = MultiAttr (l <> [r])
  append l (MultiAttr r) = MultiAttr ([l] <> r)
  append l r = MultiAttr [l,r]
instance monoidAttrProp :: Monoid AttrProp where
  mempty = MultiAttr []

data PropVal
  = PropString String
  | PropBoolean Boolean
  | PropInt Int
  | PropNumber Number

renderProps :: Element -> Lake AttrProp -> Effect (Effect Unit)
renderProps el stream = do
  listeners <- ordMap
  noLongerSelf <- accumulator
  let
    receive = case _ of
      Self withSelf -> noLongerSelf.put =<< withSelf el
      MultiAttr nested -> traverse_ receive nested
      -- important for xmlns at least on firefox apparently
      -- https://stackoverflow.com/questions/35057909/difference-between-setattribute-and-setattributensnull#comment135086722_45548128
      Attr Nothing name val -> setAttribute (AttrName name) val el
      -- ugh
      Attr (Just ns) name val -> do
        case Element.namespaceURI el == Just (NamespaceURI ns) of
          true -> setAttribute (AttrName name) val el
          false -> setAttributeNS (Just (NamespaceURI ns)) (AttrName name) val el
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
            r <- _.destroy <$> start_ do
              eventListener
                { eventPhase: Bubbling
                , eventTarget: Element.toEventTarget el
                , eventType: EventType ty
                } cb
            listeners.set ty r
  { destroy: unsubscribe } <- start do subscribe stream receive
  pure do
    unsubscribe
    listeners.reset >>= traverse_ identity
    join noLongerSelf.get

type RndrMgr =
  { doc :: Document
  , domId :: Allocar String
  , renderThread :: Lake ~> Lake
  , renderNow :: Effect Unit
  }

globalDomId :: Allocar Int
globalDomId = unsafeAllocate freshId

renderManager :: Allocar RndrMgr
renderManager = do
  doc <- window >>= document >== HTMLDocument.toDocument
  prefix <- globalDomId <#> \i j -> show i <> "." <> show j
  domId <- map prefix <$> freshId
  animFrameBuffer <- mkAnimFrameBuffer
  pure
    { doc
    , domId
    , renderThread: (animFrameBuffer.buffering :: Lake ~> Lake)
    , renderNow: animFrameBuffer.drain
    }

mkBookmarks :: RndrMgr -> (Node -> Effect Unit) -> Effect (Tuple (Pair Comment) (Node -> Effect Unit))
mkBookmarks mgr insert = do
  this <- mgr.domId -- for hydration?
  bookmarks@(Pair _ bookend) <- for (Pair false true) \side -> do
    let
      name = this <> case side of
        false -> "["
        true -> "]"
    bookmark <- createComment name mgr.doc
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
  doc <- window >>= document >== HTMLDocument.toNonElementParentNode
  getElementById id doc >>= case _ of
    Nothing -> mempty <$ do
      Console.error $ "Could not find element by id " <> show (unwrap id)
    Just el -> renderEl el dragon

renderEl :: Element -> Dragon -> Effect (Effect Unit)
renderEl el dragon = do
  mgr <- renderManager
  _.destroy <$> runEffectFn3 renderDragonIn mgr (appendChild <@> Element.toNode el) dragon <* mgr.renderNow

renderElSt :: forall flow. Stream flow Element -> Dragon -> Effect (Effect Unit)
renderElSt stream dragon = do
  destroyLast <- rolling
  { destroy: unsub } <- start $ subscribe stream \el -> do
    destroyLast =<< renderEl el dragon
  pure $ unsub <> destroyLast mempty

renderDragonIn :: EffectFn3 RndrMgr (Node -> Effect Unit) Dragon Rendered
renderDragonIn = mkEffectFn3 \mgr insert -> case _ of
  Egg egg -> start egg >>=
    \{ result: dragon, destroy } -> runEffectFn3 renderDragonIn mgr insert dragon <#>
      \r -> r { destroy = r.destroy <> destroy }
  Text changingValue -> do
    el <- Text.toNode <$> createTextNode "" mgr.doc
    { destroy: unsubscribe } <- start $
      subscribe (mgr.renderThread changingValue) \newValue -> do
        setTextContent newValue el
    insert el
    pure $ only el do
      unsubscribe
      removeSelf el
  Element ns ty props children -> do
    let create = maybe createElement (createElementNS <<< Just <<< NamespaceURI)
    el <- create ns (ElementName ty) mgr.doc
    unSubscribeProps <- renderProps el (mgr.renderThread props)
    destroyChildren <- runEffectFn3 renderDragonIn mgr (appendChild <@> Element.toNode el) children
    insert (Element.toNode el)
    pure $ only (Element.toNode el) do
      unSubscribeProps
      destroyChildren.destroy
      removeSelf (Element.toNode el)
  Fragment [child] -> runEffectFn3 renderDragonIn mgr insert child
  Fragment children ->
    case NEA.fromArray children of
      Just children' -> runEffectFn3 renderDragonsIn mgr insert children'
      _ -> do
        this <- mgr.domId -- for hydration?
        bookmark <- createComment this mgr.doc
        pure $ (only <*> removeSelf) (Comment.toNode bookmark)
  Replacing revolving -> do
    Tuple bookmarks insert' <- mkBookmarks mgr insert
    unsubPrev <- rolling
    let inactive = for_ bookmarks (Comment.toNode >>> removeSelf)
    { destroy: unsubscribe } <- start $
      subscribeIsh inactive (mgr.renderThread revolving) \newValue -> do
        unsubPrev mempty
        unsubPrev <<< _.destroy =<< runEffectFn3 renderDragonIn mgr insert' newValue
    pure $ fromBookmarks bookmarks do
      unsubscribe
      unsubPrev mempty
      inactive
  Appending items -> do
    Tuple bookmarks insert' <- mkBookmarks mgr insert
    unsubAll <- accumulator
    let inactive = for_ bookmarks (Comment.toNode >>> removeSelf)
    { destroy: unsubscribe } <- start $
      subscribeIsh inactive (mgr.renderThread items) \newChild -> do
        unsubAll.put <<< _.destroy =<< runEffectFn3 renderDragonIn mgr insert' newChild
    pure $ fromBookmarks bookmarks do
      unsubscribe
      join (unsubAll.reset)
      inactive

renderDragonsIn :: EffectFn3 RndrMgr (Node -> Effect Unit) (NonEmptyArray Dragon) Rendered
renderDragonsIn = mkEffectFn3 \mgr insert children' -> do
  let children = NEA.uncons children'
  first <- runEffectFn3 renderDragonIn mgr insert children.head
  case children.tail of
    [] -> pure first
    _ -> do
      destroyers <- pushArray
      destroyers.push first.destroy
      last <- prealloc first.tail
      foreachE children.tail \child -> do
        r <- runEffectFn3 renderDragonIn mgr insert child
        destroyers.push r.destroy
        last.set r.tail
      lastLast <- last.get
      pure
        { head: first.head
        , destroy: flip foreachE identity =<< destroyers.reset
        , tail: lastLast
        }

-- | Take a snapshot of a DOM element as a static `Dragon`.
snapshot :: Web.Element -> Effect Dragon
snapshot e = do
  attrs <- Element.attributes e >>= getAttributes >>= traverse \attr -> do
    value <- getValue attr
    pure $ Attr (coerce Attr.namespaceURI attr) (coerce Attr.localName attr) value
  kids <- children (Element.toParentNode e) >>= HTMLCollection.toArray >>= traverse snapshot
  pure $ Element
    (coerce Element.namespaceURI e)
    (coerce Element.tagName e)
    (bursting attrs)
    (Fragment kids)

