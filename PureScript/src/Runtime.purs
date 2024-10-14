module Runtime where

import Prelude

import Data.Argonaut (Json, encodeJson)
import Data.Argonaut as Json
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, try)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (catchException)
import Fetch (fetch)
import Parser.Comb.Comber (ParseError, thaw)
import Riverdragon.River (Lake)
import Riverdragon.River.Beyond (affToLake)
import Runtime.Types (NamedParser(..))
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage

-- tailRecM

-- loopSome ::

-- parseRegexLoop ::

configurable :: forall m. MonadEffect m => String -> String -> m String
configurable name default = liftEffect do
  try (Storage.getItem name =<< localStorage =<< window) >>= case _ of
    Left _ -> pure default
    Right (Nothing) -> pure default
    Right (Just r) -> pure r

fetchText :: String -> Aff String
fetchText url = _.text =<< fetch url {}

cacheAff ::
  forall error cached.
  CacheFor cached ->
  Array String ->
  Aff (Either error cached) ->
  Aff (Either error cached)
cacheAff cache key fallback = do
  liftEffect (cache.get key) >>= case _ of
    Just cached -> pure (Right cached)
    Nothing -> fallback >>= case _ of
      Left err -> pure (Left err)
      Right cached -> Right cached <$ liftEffect (cache.set key cached)

obtainParser :: forall a. NamedParser a -> Lake (String -> Either ParseError a)
obtainParser (NamedParser name parser) =
  thaw parser <<< fromMaybe (Left unit) <$> affToLake do
    let fullUrl = "assets/json/" <> name <> "-parser-states.json"
    cacheAff _parserCache [name] do
      lmap (const unit) <<< Json.parseJson <$> fetchText fullUrl

_parserCache :: CacheFor Json
_parserCache = specialCache (Proxy :: Proxy Json) "parser-states"

type CacheFor cached =
  { get :: Array String -> Effect (Maybe cached)
  , set :: Array String -> cached -> Effect Unit
  }

specialCache ::
  forall cached.
    Proxy cached ->
    String ->
    CacheFor cached
specialCache _ name =
  let keyed key = Json.stringify (encodeJson ([ name ] <> key)) in
  { get: keyed >>> \cacheKey -> catchException (const (pure Nothing)) do
      pure unit
      Just <$> _specialCacheGet cacheKey
  , set: keyed >>> \cacheKey value -> do
      pure unit
      _specialCacheSet cacheKey value
  }

foreign import _specialCacheSet :: forall cached. String -> cached -> Effect Unit
foreign import _specialCacheGet :: forall cached. String -> Effect cached

aSideChannel ::
  forall sideChannel.
    Proxy sideChannel ->
    String ->
    { messageInABottle :: sideChannel -> Effect Unit
    , installChannel :: (sideChannel -> Effect Unit) -> Effect Unit
    }
aSideChannel _ name =
  { messageInABottle: \msg -> do
      pure unit
      _messageInABottle name msg
  , installChannel: \cb -> do
      pure unit
      _installSideChannel name cb
  }

foreign import _messageInABottle :: forall sideChannel. String -> sideChannel -> Effect Unit
foreign import _installSideChannel :: forall sideChannel. String -> (sideChannel -> Effect Unit) -> Effect Unit
