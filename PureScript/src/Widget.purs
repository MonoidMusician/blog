module Widget where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global (Global)
import Control.Monad.ST.Internal as STRef
import Control.Plus (empty)
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Codec (BasicCodec, decode, encode)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, for_, oneOfMap, traverse_)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Control (fixed)
import Deku.Toplevel (runInElement')
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, create, filterMap, keepLatest, makeEvent, subscribe, sweep)
import FRP.Event.Class (bang)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STO
import Web.DOM (Element)
import Web.DOM.Document as Document
import Web.DOM.Element (getAttribute, removeAttribute)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node (appendChild, removeChild)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (cancelAnimationFrame, document, requestAnimationFrame)
import Widget.Types (SafeNut(..))

type Interface a =
  { send :: a -> Effect Unit
  , receive :: Event a
  , loopback :: Event a
  , mailbox :: Event (a -> Event Unit)
  , current :: Effect (Maybe a)
  }
disconnected :: forall a. Interface a
disconnected =
  { send: mempty
  , receive: empty
  , loopback: empty
  , mailbox: bang (const empty)
  , current: pure Nothing
  }

type KeyedInterface = String -> Interface Json
-- We adjoint integers for tracking sources so we do not send to the source
-- it came from
type DataShare = STObject Global (Int /\ (String -> Interface (Int /\ Json)))

makeKeyedInterface :: forall a. Ord a => Effect (String -> Interface a)
makeKeyedInterface = do
  interfaces <- liftST $ STO.new
  -- This is possible to do purely, by creating a shared event bus in the
  -- original effect execution, but this way is more efficient
  pure \k -> unsafePerformEffect do
    existing <- liftST (STO.peek k interfaces)
    case existing of
      Just r -> pure r
      Nothing -> do
        io <- createBehavioral
        io <$ liftST (STO.poke k io interfaces)

-- Each new subscription gets the latest value
createBehavioral :: forall a. Ord a => Effect (Interface a)
createBehavioral = do
  ref <- liftST (STRef.new Nothing)
  upstream <- create
  let
    write a = void $ liftST (STRef.write (Just a) ref)
    downstream = makeEvent \k -> do
      liftST (STRef.read ref) >>= traverse_ k
      subscribe upstream.event k
  pure
    { send: write <> upstream.push
    , receive: downstream
    , loopback: downstream
    , mailbox: sweep downstream identity
    , current: liftST (STRef.read ref)
    }

getInterface :: DataShare -> String -> Effect KeyedInterface
getInterface share k = do
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
      receive = filterMap (\(j /\ v) -> if i /= j then Just v else Nothing) int.receive
    in
      { send: \v -> int.send (i /\ v)
      , receive: receive
      , loopback: int.receive <#> snd
      , mailbox: sweep receive identity
      , current: int.current <#> map \(_ /\ v) -> v
      }

adaptInterface :: forall f a b. Foldable f => BasicCodec f a b -> Interface a -> Interface b
adaptInterface codec interface =
  { send: interface.send <<< encode codec
  , mailbox: interface.mailbox <#> (_ <<< encode codec)
  , receive: keepLatest $ oneOfMap bang <<< decode codec <$> interface.receive
  , loopback: keepLatest $ oneOfMap bang <<< decode codec <$> interface.loopback
  , current: interface.current <#> bindFlipped
      (decode codec >>> last)
  }
  where
  last = unwrap <<< foldMap (Last <<< Just)

type KeyedInterfaceWithAttrs =
  { interface :: KeyedInterface
  , attrs :: String -> Effect Json
  }

type Widget = KeyedInterfaceWithAttrs -> Effect SafeNut
type Widgets = Object Widget

-- <div data-widget="Parser.Grammar" data-widget-datakey="default or lab1 or sandboxed" data-widget-data-grammar="add-sub-mul-exp"></div>

lookupWidget :: Widgets -> Element -> Effect Widget
lookupWidget widgets target = do
  mname <- getAttribute "data-widget" target
  let
    mwidget = mname >>= \name -> Object.lookup name widgets
  case mwidget of
    Nothing -> do
      case mname of
        Nothing -> log $ "Missing widget (no name)"
        Just name -> log $ "Missing widget " <> show name
      pure $ pure $ pure $ SafeNut (fixed empty)
    Just widget -> pure widget

lookupInterface :: DataShare -> Element -> Effect KeyedInterface
lookupInterface share target =
  getAttribute "data-widget-datakey" target >>=
    case _ of
      Nothing -> pure (const disconnected)
      Just name -> do
        getInterface share name

collectAttrs :: Element -> Effect (String -> Effect Json)
collectAttrs target = pure \name ->
  getAttribute ("data-widget-data-" <> name) target <#> case _ of
    Nothing -> Json.jsonNull
    Just astr ->
      case Json.parseJson astr of
        Right j -> j
        Left _ -> Json.fromString astr

instantiateWidget :: Widgets -> DataShare -> Element -> Effect (Effect Unit)
instantiateWidget widgets share target = do
  widget <- lookupWidget widgets target
  interface <- lookupInterface share target
  getAttribute "data-widget-parent" target >>= traverse_ \search ->
    window >>= document >>= HTMLDocument.toParentNode >>> querySelectorAll (QuerySelector search) >>= NodeList.toArray >>= case _ of
      [newParent] -> appendChild (Element.toNode target) newParent
      [] -> log $ "No elements found matching selector " <> search
      _ -> log $ "Multiple elements found matching selector " <> search
  attrs <- collectAttrs target
  safenut <- widget { interface, attrs }
  unsub <- snd <$> runInElement' target case safenut of
    SafeNut nut -> nut
  removeAttribute "data-widget-loading" target
  pure $ unsub <> do
    childs <- HTMLCollection.toArray =<< (Element.toParentNode target # ParentNode.children)
    for_ childs \child ->
      Element.toNode target # removeChild (Element.toNode child)

raf :: Aff Unit
raf = makeAff \cb -> do
  id <- requestAnimationFrame (cb (Right unit)) =<< window
  pure $ Canceler \_ -> liftEffect (cancelAnimationFrame id =<< window)

instantiateAll :: Widgets -> Effect (Effect Unit)
instantiateAll widgets = do
  d <- window >>= document >>= HTMLDocument.toDocument >>> pure
  share <- liftST STO.new
  targetsN <- NodeList.toArray =<< querySelectorAll (QuerySelector "[data-widget]") (Document.toParentNode d)
  let targetsE = Array.mapMaybe Element.fromNode targetsN
  unsub <- Ref.new mempty
  launchAff_ $ for_ targetsE \target -> do
    raf
    u <- liftEffect $ instantiateWidget widgets share target
    liftEffect $ Ref.modify_ (_ <> u) unsub
  pure $ join $ Ref.read unsub
