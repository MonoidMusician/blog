module Riverdragon.Dragon where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Pair (Pair(..))
import Data.Semigroup.First (First(..))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup.Last (Last(..))
import Data.Traversable (for, for_, sequence_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Uncurried (runEffectFn3)
import Idiolect ((>==))
import Riverdragon.Dragon.Breath (removeSelf, setAttributeNS, unsafeSetProperty)
import Riverdragon.River (Lake, subscribe, subscribeIsh)
import Riverdragon.River.Bed (Allocar, accumulator, eventListener, freshId, ordMap, rolling, unsafeAllocate)
import Web.DOM (Comment, Document, Element, Node)
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Comment as Comment
import Web.DOM.Document (createComment, createElement, createElementNS, createTextNode)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.ElementId (ElementId)
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
  | Egg (Effect { dragon :: Dragon, destroy :: Effect Unit })
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
  append l r = Fragment [l,r]
instance monoidDragon :: Monoid Dragon where
  mempty = Fragment []

data AttrProp
  = Attr (Maybe String) String String
  | Prop String PropVal
  | Listener String (Maybe (Web.Event -> Effect Unit))
  | Self (Element -> Effect (Effect Unit))

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
            let
              doIt = eventListener
                { eventPhase: Bubbling
                , eventTarget: Element.toEventTarget el
                , eventType: EventType ty
                }
            r <- doIt cb
            listeners.set ty r
  unsubscribe <- subscribe stream receive
  pure do
    unsubscribe
    listeners.reset >>= traverse_ identity
    join noLongerSelf.get

type RndrMgr =
  { doc :: Document
  , domId :: Allocar String
  }

globalDomId :: Allocar Int
globalDomId = unsafeAllocate freshId

renderManager :: Allocar RndrMgr
renderManager = do
  doc <- window >>= document >== HTMLDocument.toDocument
  prefix <- globalDomId <#> \i j -> show i <> "." <> show j
  domId <- map prefix <$> freshId
  pure { doc, domId }

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
  _.destroy <$> renderTo mgr (appendChild <@> Element.toNode el) dragon

renderTo :: RndrMgr -> (Node -> Effect Unit) -> Dragon -> Effect Rendered
renderTo mgr insert (Egg egg) = egg >>=
  \{ dragon, destroy } -> renderTo mgr insert dragon <#>
    \r -> r { destroy = r.destroy <> destroy }
renderTo mgr insert (Text changingValue) = do
  el <- createTextNode "" mgr.doc
  unsubscribe <- subscribeIsh mempty changingValue \newValue -> do
    setTextContent newValue (Text.toNode el)
  insert (Text.toNode el)
  pure $ only (Text.toNode el) do
    unsubscribe
    removeSelf (Text.toNode el)
renderTo mgr insert (Element ns ty props children) = do
  let create = maybe createElement (createElementNS <<< Just <<< NamespaceURI)
  el <- create ns (ElementName ty) mgr.doc
  unSubscribeProps <- renderProps el props
  destroyChildren <- renderTo mgr (appendChild <@> Element.toNode el) children
  insert (Element.toNode el)
  pure $ only (Element.toNode el) do
    unSubscribeProps
    destroyChildren.destroy
    removeSelf (Element.toNode el)
renderTo mgr insert (Fragment children) =
  case NEA.fromArray children of
    Nothing -> do
      this <- mgr.domId -- for hydration?
      bookmark <- createComment this mgr.doc
      pure $ (only <*> removeSelf) (Comment.toNode bookmark)
    Just children' -> foldMap1 (renderTo mgr insert) children'
renderTo mgr insert (Replacing revolving) = do
  Tuple bookmarks insert' <- mkBookmarks mgr insert
  unsubPrev <- rolling
  let inactive = for_ bookmarks (Comment.toNode >>> removeSelf)
  unsubscribe <- subscribeIsh (mempty inactive) revolving \newValue -> do
    unsubPrev mempty
    unsubPrev <<< _.destroy =<< renderTo mgr insert' newValue
  pure $ fromBookmarks bookmarks do
    unsubscribe
    unsubPrev mempty
    inactive
renderTo mgr insert (Appending items) = do
  Tuple bookmarks insert' <- mkBookmarks mgr insert
  unsubAll <- accumulator
  let inactive = for_ bookmarks (Comment.toNode >>> removeSelf)
  unsubscribe <- subscribeIsh (mempty inactive) items \newChild -> do
    unsubAll.put <<< _.destroy =<< renderTo mgr insert' newChild
  pure $ fromBookmarks bookmarks do
    unsubscribe
    join (unsubAll.reset)
    inactive
