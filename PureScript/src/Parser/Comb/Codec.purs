module Parser.Comb.Codec where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Profunctor (dimap)
import Data.Variant as Variant
import Parser.Codecs (_n, shiftReduceCodec)
import Parser.Comb.Run as CombR
import Parser.Lexing (type (~), Rawr, unRawr)
import Parser.Lexing as Lex
import Parser.Types (OrEOF(..), Part(..), Zipper(..), notEOF)
import Type.Proxy (Proxy(..))

type CStates = CombR.CStates String (String ~ Rawr)
type StateTable =
  { stateMap :: Map String Int
  , start :: Int
  , states :: CStates
  }

tokenCodec :: JsonCodec (OrEOF (String ~ Rawr))
tokenCodec = dimap notEOF (maybe EOF Continue) $ CAC.maybe $
  _Newtype $ CAC.either CA.string $ dimap unRawr Lex.rawr CA.string

fragmentCodec :: JsonCodec (Array (Part (Either String String) (OrEOF (String ~ Rawr))))
fragmentCodec = CA.array $ CAV.variantMatch
  { "Terminal": Right tokenCodec
  , "NonTerminal": Right (CAC.either CA.string CA.string)
  } # dimap
    do
      case _ of
        Terminal s -> Variant.inj (Proxy :: Proxy "Terminal") s
        NonTerminal r -> Variant.inj (Proxy :: Proxy "NonTerminal") r
    do
      Variant.match
        { "Terminal": Terminal
        , "NonTerminal": NonTerminal
        }

indexedCodec :: forall k a. Ord k => JsonCodec k -> JsonCodec a -> JsonCodec (Map.Map k a)
indexedCodec k a = dimap Map.toUnfoldable Map.fromFoldable $ CA.array $ CAC.tuple k a

stateTableCodec :: JsonCodec StateTable
stateTableCodec = CAR.object "StateTable"
  { stateMap: indexedCodec CA.string CA.int
  , start: CAC.int
  , states: _n $ CA.array $
      CAR.object "State"
        { sName: CA.int
        , items: _n $ CA.array $ CAR.object "StateItem"
          { rName: CAC.maybe CA.int
          , pName: CAC.either CA.string CA.string
          , rule: CA.indexedArray "Zipper" $ Zipper
              <$> (\(Zipper before _) -> before) CA.~ CA.index 0 fragmentCodec
              <*> (\(Zipper _ after) -> after) CA.~ CA.index 1 fragmentCodec
          , lookahead: CA.array tokenCodec
          }
        , advance: _n $ indexedCodec tokenCodec $
            shiftReduceCodec CA.int $
              CAC.tuple (CAC.either CA.string CA.string) (CAC.maybe CA.int)
        , receive: indexedCodec (CAC.either CA.string CA.string) CA.int
        }
  }
