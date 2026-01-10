module Widget where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.ResourceM (destr, selfDestructor, track_)
import Control.Monad.ResourceT (ResourceM, start, start_)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Plus (empty)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Enum (fromEnum)
import Data.Filterable (filterMap)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STO
import Idiolect (JSON, filterFst, (>==))
import Parser.Printer.JSON as C
import Riverdragon.Dragon (Dragon, renderEl, snapshot)
import Riverdragon.Dragon.Bones as Dragon
import Riverdragon.River (Allocar, River, createRiverStore, createStore, mailbox, mailboxRiver, stillRiver)
import Riverdragon.River.Bed (allocLazy)
import Web.DOM (Element, ParentNode)
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Document as Document
import Web.DOM.Element (getAttribute, removeAttribute, setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (Node, appendChild, nodeTypeIndex, removeChild, textContent)
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (cancelAnimationFrame, document, requestAnimationFrame, sessionStorage)
import Web.Storage.Storage as Storage

-- | An interface to a mutable value, stored in one location. It may not have
-- | a value at first, or ever.
type Interface a =
  { send :: a -> Effect Unit
  , receive :: River a
  , loopback :: River a
  , mailbox :: a -> River Unit
  , current :: Effect (Maybe a)
  , destroy :: Allocar Unit
  }
disconnected :: forall a. Interface a
disconnected =
  { send: mempty
  , receive: empty
  , loopback: empty
  , mailbox: \_ -> empty
  , current: pure Nothing
  , destroy: mempty
  }

stillInterface :: forall a.
  { send :: a -> Effect Unit
  , receive :: River a
  , loopback :: River a
  , mailbox :: a -> River Unit
  , current :: Effect (Maybe a)
  , destroy :: Allocar Unit
  } -> Interface a
stillInterface i =
  { send: i.send
  , receive: stillRiver i.receive
  , loopback: stillRiver i.loopback
  , mailbox: \k -> stillRiver (i.mailbox k)
  , current: i.current
  , destroy: i.destroy
  }

storeInterface :: forall a. Ord a => Allocar (Interface a)
storeInterface = _.result <$> start "storeInterface" do
  destroy <- selfDestructor
  stream <- createRiverStore Nothing
  byKey <- mailbox (map { key: _, value: unit } stream.stream)
  pure $ stillInterface
    { send: stream.send
    -- Here we don't have a notion of actors, so receive = loopback,
    -- but downstream interfaces are able to adapt it to their needs
    , receive: stream.stream
    , loopback: stream.stream
    , mailbox: byKey
    , current: stream.current
    , destroy
    }

valueInterface :: forall a. Ord a => a -> Allocar (Interface a)
valueInterface v0 = _.result <$> start "valueInterface" do
  destroy <- selfDestructor
  stream <- createStore v0
  byKey <- mailbox (map { key: _, value: unit } stream.stream)
  pure $ stillInterface
    { send: stream.send
    -- Here we don't have a notion of actors, so receive = loopback,
    -- but downstream interfaces are able to adapt it to their needs
    , receive: stream.stream
    , loopback: stream.stream
    , mailbox: byKey
    , current: Just <$> stream.current
    , destroy
    }


makeKeyedInterface :: forall a. Ord a => Allocar (String -> Interface a)
makeKeyedInterface = allocLazy do
  interfaces <- liftST STO.new
  pure \k -> do
    -- This is possible to do purely, by creating a shared event bus in the
    -- original effect execution, but this way is more efficient
    existing <- liftST (STO.peek k interfaces)
    case existing of
      Just r -> pure r
      Nothing -> do
        -- log $ "Make interface " <> show k
        io <- storeInterface
        io <$ liftST (STO.poke k io interfaces)

-- | TODO: make it truly tab-global? interconnect between different instances
sessionStorageInterface :: Allocar (String -> Interface String)
sessionStorageInterface = allocLazy do
  storage <- window >>= sessionStorage
  pure \key -> _.result <$> start "sessionStorageInterface" do
    destroy <- selfDestructor
    stream <- createRiverStore =<< liftEffect do Storage.getItem key storage
    byKey <- mailbox (map { key: _, value: unit } stream.stream)
    pure $ stillInterface
      { send: (Storage.setItem key <@> storage) <> stream.send
      -- Here we don't have a notion of actors, so receive = loopback,
      -- but downstream interfaces are able to adapt it to their needs
      , receive: stream.stream
      , loopback: stream.stream
      , mailbox: byKey
      , current: stream.current
      , destroy
      }

type KeyedInterface = String -> Interface Json
-- We adjoin integers for tracking sources so we do not send to the source
-- it came from
type DataShare = STObject Global (Int /\ (String -> Interface (Int /\ Json)))


obtainInterface :: DataShare -> String -> Effect KeyedInterface
obtainInterface share k = do
  existing <- liftST (STO.peek k share)
  i /\ r <- case existing of
    Just (i /\ r) -> do
      (i /\ r) <$ liftST (STO.poke k ((i+1) /\ r) share)
    Nothing -> do
      r <- makeKeyedInterface
      (0 /\ r) <$ liftST (STO.poke k (1 /\ r) share)
  pure \k1 ->
    let
      int = r k1
      receive = filterMap (filterFst (notEq i)) int.receive
    in stillInterface
      { send: \v -> int.send (i /\ v)
      , receive: receive
      , loopback: int.receive <#> snd
      , mailbox: mailboxRiver (map { value: unit, key: _ } receive)
      , current: int.current <#> map \(_ /\ v) -> v
      -- We are not allowed to destroy from a datashare
      , destroy: mempty
      }

adaptInterface :: forall ctx err @b @a. Monoid ctx => C.OneCodec ctx err Void a a b b -> Interface a -> Interface b
adaptInterface codec interface =
  { send: interface.send <<< C.encode codec
  , mailbox: interface.mailbox <<< C.encode codec
  , receive: filterMap (hush <<< C.decode' codec) interface.receive
  , loopback: filterMap (hush <<< C.decode' codec) interface.loopback
  , current: interface.current <#> bindFlipped
      (C.decode' codec >>> hush)
  , destroy: interface.destroy
  }

adaptInterfaceR :: forall @a. C.JCodec a -> Interface JSON -> Interface a
adaptInterfaceR = adaptInterface <<< C.registration C.autoJ

autoAdaptInterface :: forall @a. C.AutoJSON a JSON => Interface JSON -> Interface a
autoAdaptInterface = adaptInterfaceR C.autoJ

type KeyedInterfaceWithAttrs =
  { interface :: KeyedInterface
  , attrs :: String -> Effect Json
  , rawAttr :: AttrName -> Effect (Maybe String)
  , text :: Effect String
  , content :: Effect Dragon
  }

type Widget = KeyedInterfaceWithAttrs -> ResourceM Dragon
type Widgets = Object Widget

-- <div data-widget="Parser.Grammar" data-widget-datakey="default or lab1 or sandboxed" data-widget-data-grammar="add-sub-mul-exp"></div>

lookupWidget :: Widgets -> Element -> Effect Widget
lookupWidget widgets target = do
  mname <- getAttribute (AttrName "data-widget") target
  let
    mwidget = mname >>= \name -> Object.lookup name widgets
  case mwidget of
    Nothing -> do
      case mname of
        Nothing -> log $ "Missing widget (no name)"
        Just name -> log $ "Missing widget " <> show name
      pure $ pure $ pure mempty
    Just widget -> pure widget

lookupInterface :: DataShare -> Element -> Effect KeyedInterface
lookupInterface share target =
  getAttribute (AttrName "data-widget-datakey") target >>=
    case _ of
      Nothing -> pure (const disconnected)
      Just name -> do
        obtainInterface share name

collectAttrs :: Element -> Effect (String -> Effect Json)
collectAttrs target = pure \name ->
  getAttribute (AttrName ("data-widget-data-" <> name)) target <#> case _ of
    Nothing -> Json.jsonNull
    Just astr ->
      case Json.parseJson astr of
        Right j -> j
        Left _ -> Json.fromString astr

instantiateWidget :: Widgets -> DataShare -> Element -> Effect (Effect Unit)
instantiateWidget widgets share target = _.destroy <$> start_ "instantiateWidget" do
  widget <- liftEffect do lookupWidget widgets target
  interface <- liftEffect do lookupInterface share target
  attrs <- liftEffect do collectAttrs target
  -- data-widget-parent lets it teleport to a different spot on the page
  -- based on the specified selector
  liftEffect do
    getAttribute (AttrName "data-widget-parent") target >>= traverse_ \search ->
      window >>= document >>= HTMLDocument.toParentNode >>> querySelectorAll (QuerySelector search) >>= NodeList.toArray >>= case _ of
        [newParent] -> appendChild (Element.toNode target) newParent
        [] -> log $ "No elements found matching selector " <> search
        _ -> log $ "Multiple elements found matching selector " <> search
  -- call the widget code
  dragon <- widget
    { interface
    , attrs
    , rawAttr: getAttribute <@> target
    , text: textContent (Element.toNode target)
    , content: do
        snapshots <- forElementChildNodes target \node -> do
          case Element.fromNode node of
            Just e -> Just <$> snapshot e
            Nothing
              | nodeTypeIndex node == fromEnum TextNode -> do
                t <- Node.textContent node
                pure $ Just $ Dragon.text t
              | otherwise -> pure Nothing
        removeChildNodes target
        pure $ Array.fold $ Array.catMaybes snapshots
    }
  -- run the tree in the target
  track_ do renderEl target dragon

  -- final stuff
  liftEffect do removeAttribute (AttrName "data-widget-loading") target
  destr do removeChildNodes target

raf :: Aff Unit
raf = makeAff \cb -> do
  id <- requestAnimationFrame (cb (Right unit)) =<< window
  pure $ Canceler \_ -> liftEffect (cancelAnimationFrame id =<< window)

removeChildNodes :: Element -> Effect Unit
removeChildNodes target =
  forElementChildNodes_ target \child ->
    Element.toNode target # removeChild child

forElementChildNodes_ :: forall a. Element -> (Node -> Effect a) -> Effect Unit
forElementChildNodes_ target fn = do
  childs <- Node.childNodes (Element.toNode target) >>= NodeList.toArray
  for_ childs fn

forElementChildNodes :: forall a. Element -> (Node -> Effect a) -> Effect (Array a)
forElementChildNodes target fn = do
  childs <- Node.childNodes (Element.toNode target) >>= NodeList.toArray
  for childs fn

queryElements :: QuerySelector -> ParentNode -> Effect (Array Element)
queryElements query parent =
  querySelectorAll query parent
    >>= NodeList.toArray
    >== Array.mapMaybe Element.fromNode

instantiateAll :: Widgets -> Effect (Effect Unit)
instantiateAll widgets = do
  d <- window >>= document >== HTMLDocument.toDocument >>> Document.toParentNode
  share <- liftST STO.new
  -- Add data-widget to those that only have data-t
  queryElements (QuerySelector "[data-t]:not(data-widget)") d >>=
    traverse_ (setAttribute (AttrName "data-widget") "")
  -- Collect all the widgets
  targets <- queryElements (QuerySelector "[data-widget]") d
  -- Make a ref to accumulate unsubscribing actions
  unsub <- Ref.new mempty
  -- Asynchronously instantiate widgets
  -- I guess it should be cancellable for edge cases? eh
  launchAff_ $ for_ targets \target -> do
    raf
    u <- liftEffect $ instantiateWidget widgets share target
    liftEffect $ Ref.modify_ (_ <> u) unsub
  pure $ join $ Ref.read unsub
