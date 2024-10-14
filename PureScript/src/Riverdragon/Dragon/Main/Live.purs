module Riverdragon.Dragon.Main.Live where

import Prelude

import Ansi.Codes as Ansi
import Control.Monad.Except (runExcept)
import Control.Plus ((<|>))
import Data.Array (intercalate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, traverse_)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, message, runAff, throwError)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Ref as Ref
import Fetch (Method(..), fetch)
import Foreign (readArray, readString)
import Foreign.Index (readProp)
import Idiolect (intercalateMap, (<#?>), (>==))
import JSURI (encodeURIComponent)
import Parser.Comb.Comber (Comber, Printer, parse, printGrammarWsn, toAnsi)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderEl)
import Riverdragon.Dragon.Bones ((.$), (.$$~), (:.), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode)
import Riverdragon.River (Lake, createRiverStore, makeLake, subscribe)
import Riverdragon.River.Beyond (dedup)
import Runtime (aSideChannel)
import Tidy.Codegen as TC
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Widget (Widget)

mainForDragon :: Dragon -> Effect Unit
mainForDragon dragon = do
  log "Loading"
  _sideChannel.messageInABottle
    { renderToEl: renderEl <@> dragon
    }

type SideChannel =
  { renderToEl :: Element -> Effect (Effect Unit)
  }

_sideChannel = aSideChannel (Proxy :: Proxy SideChannel) "Riverdragon.Dragon.Main.Live"

sideChannel :: Lake SideChannel
sideChannel = makeLake \cb -> mempty <$ _sideChannel.installChannel cb
