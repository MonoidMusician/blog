module Parser.Main.Comb where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BooleanAlgebra.CSS (Vert, combineFold, printVerts, subsumptite)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), blush, hush)
import Data.Filterable (filter, filterMap)
import Data.Foldable (fold, foldMap, oneOf, oneOfMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra (tt)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=))
import Deku.Control (text_)
import Deku.Control as DC
import Deku.Core (Domable, bussed, envy)
import Deku.DOM as D
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Console (log)
import FRP.Aff (affToEvent)
import FRP.Event (Event, makeEvent, memoize)
import FRP.Memoize (memoBehFold, memoLast)
import Fetch (fetch)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parser.Languages (Comber)
import Parser.Languages.CSS (mkCSSParser)
import Parser.Main (inputValidated)
import Parser.Template (template)
import PureScript.CST as CST
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Types as CST.T
import Tidy (toDoc)
import Tidy as Tidy
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

mainForParser :: Comber (Dodo.Doc Void) -> Effect Unit
mainForParser parser = pure unit

applyTemplate :: CST.T.Expr Void -> CST.T.Module Void
applyTemplate parserExpr = template #
  ( _Newtype <<< prop (Proxy :: Proxy "body")
  <<< _Newtype <<< prop (Proxy :: Proxy "decls")
  <<< traversed
  ) case _ of
    CST.T.DeclValue decl@{ name: CST.T.Name { name: CST.T.Ident "parser" } } ->
      CST.T.DeclValue decl { guarded = guarded }
      where
      fake =
        join { start: _, end: _ } $
          join { line: _, column: _ } 0
      tok value =
        { range: fake
        , leadingComments: []
        , trailingComments: []
        , value
        }
      guarded =
        CST.T.Unconditional (tok CST.T.TokEquals) $
          CST.T.Where
            { bindings: Nothing
            , expr: parserExpr
            }
    decl -> decl

assemble :: String -> Either (NonEmptyArray CST.M.PositionedError) String
assemble = CST.parseExpr >>> case _ of
  CST.ParseSucceeded parserExpr -> Right do
    Dodo.print Dodo.plainText Dodo.twoSpaces $ toDoc $
      Tidy.formatModule Tidy.defaultFormatOptions $ applyTemplate parserExpr
  CST.ParseSucceededWithErrors _ es -> Left es
  CST.ParseFailed e -> Left (pure e)


