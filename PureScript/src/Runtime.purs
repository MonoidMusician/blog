module Runtime where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (tailRecM)
import Data.Argonaut as Json
import Data.Either (Either)
import Data.Foldable (fold)
import FRP.Aff (affToEvent)
import FRP.Event (Event)
import Fetch (fetch)
import Parser.Comb.Comber (Comber(..), ParseError, thaw)

-- tailRecM

-- loopSome ::

-- parseRegexLoop ::

asdf :: forall a. String -> Comber a -> Event (String -> Either ParseError a)
asdf url parser =
  affToEvent (_.text =<< fetch url {}) <#> \json ->
    thaw parser (Json.parseJson (fold json))

