module Widget.Query where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Semigroup.Last (Last(..))
import Data.String as String
import Data.Traversable (foldMap, for_, traverse)
import Data.Tuple.Nested ((/\))
import Deku.Control (envy, fixed)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, fromEvent, makeEvent, subscribe)
import FRP.Event.Class (bang)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parsing (ParseError(..), Parser, runParser)
import URI.Common (URIPartParseError(..))
import URI.Extra.QueryPairs as QueryPairs
import URI.Query as Query
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Widget (Widget, Interface)
import Widget.Types (SafeNut(..))

getQueryPairs :: Effect (QueryPairs.QueryPairs QueryPairs.Key QueryPairs.Value)
getQueryPairs = do
  s <- window >>= Window.location >>= Location.search
  pure case runParser s Query.parser of
    Left _ -> QueryPairs.QueryPairs []
    Right q -> case QueryPairs.parse pure pure q of
      Left _ -> QueryPairs.QueryPairs []
      Right qps -> qps

getQueryKeys :: Effect (Object Json)
getQueryKeys = map Object.fromFoldable $ getQueryPairs <#>
  \(QueryPairs.QueryPairs qps) ->
    qps <#> \(k /\ mv) ->
      QueryPairs.keyToString k /\ case QueryPairs.valueToString <$> mv of
        Nothing -> Json.jsonNull
        Just v | Right j <- Json.parseJson v -> j
        Just v -> Json.fromString v

setQueryKeys :: Object Json -> Effect Unit
setQueryKeys overwriting = do
  -- QueryPairs keeps duplicates, so we need to manually drop the keys
  -- we are overwriting
  qps <- getQueryPairs <#> \(QueryPairs.QueryPairs qps) -> qps #
    Array.filter \(k /\ _) ->
      not $ QueryPairs.keyToString k `Object.member` overwriting
  let
    overwrite = Object.toUnfoldable overwriting <#> \(k /\ v) ->
      QueryPairs.keyFromString k /\
        if Json.isNull v
          then Nothing
          else Just $ QueryPairs.valueFromString $ Json.stringify v
    q = Query.print $ QueryPairs.print identity identity $
      -- prepend changed keys just so they are more visible
      QueryPairs.QueryPairs $ overwrite <> qps
  h <- window >>= Window.history
  History.pushState (unsafeToForeign Json.jsonNull) (History.DocumentTitle "") (History.URL q) h

queryChanges :: Event (Object Json)
queryChanges = makeEvent \push -> do
  w <- window
  e <- eventListener \_ -> push =<< getQueryKeys
  addEventListener popstate e false (Window.toEventTarget w)
  pure $ removeEventListener popstate e false (Window.toEventTarget w)

widgetFromEvent :: forall a. Event a -> SafeNut
widgetFromEvent e = SafeNut (envy (fixed empty <$ fromEvent e))

sideInterface :: String -> Array { key :: String, io :: Interface Json } -> Event (Object Json)
sideInterface prefix interfaces = makeEvent \k -> do
  initial <- map (map unwrap) $ getQueryKeys <#> \q -> interfaces # foldMap \{ key } ->
    Object.lookup (prefix <> key) q # foldMap (Last >>> Object.singleton key)
  ref <- Ref.new initial
  sub1 <- interfaces # foldMap \{ key, io } -> do
    subscribe io.receive \value -> do
      setQueryKeys (Object.singleton (prefix <> key) value)
      Ref.modify_ (Object.insert key value) ref
  sub2 <- subscribe (bang initial <|> queryChanges) \kvs -> do
    for_ interfaces \{ key, io } -> do
      for_ (Object.lookup (prefix <> key) kvs) \value -> do
        io.current >>= case _ of
          Just v | v == value -> pure unit
          _ -> io.send value
        Ref.modify_ (Object.insert key value) ref
  k initial
  pure $ sub1 <> sub2

widget :: Widget
widget { interface, attrs } = do
  prefix <- attrs "prefix" <#> Json.toString >>> maybe "" (_ <> "-")
  keys <- map (fromMaybe []) $ attrs "keys" <#>
    (Json.toArray >=> traverse Json.toString) <>
    (Json.toString >>> map (String.split (String.Pattern ",") >>> map String.trim))
  let interfaces = ({ key: _, io: _ } <*> interface) <$> keys
  pure $ widgetFromEvent $ sideInterface prefix interfaces
