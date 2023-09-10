module Parser.Main.CSS where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.BooleanAlgebra.CSS (Vert, combineFold)
import Data.Either (Either, hush)
import Data.Filterable (filter, filterMap)
import Data.Foldable (fold, foldMap, oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=))
import Deku.Control (text_)
import Deku.Control as DC
import Deku.Core (Domable, bussed, envy)
import Deku.DOM as D
import FRP.Memoize (memoBehFold)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parser.Languages.CSS (mkCSSParser, printVerts)
import Parser.Main (inputValidated)
import Widget (Widget)
import Widget.Types (SafeNut(..))

type Parser = String -> Either String (Array Vert)

widgets :: Object Widget
widgets = Object.fromFoldable $
  [ "Parser.Main.CSS" /\ widgetCSS
  ]

widgetCSS :: Widget
widgetCSS _ = pure $ SafeNut do component $ mkCSSParser unit

result :: Parser -> Array String -> String
result parser = filterMap (parser >>> hush) >>> sequence >>> foldMap combineFold >>> printVerts

data Update
  = Update Int String
  | Delete Int
  | Add

component :: forall lock payload. Parser -> Domable lock payload
component parser =
  bussed \pushUpdate pushedRaw' -> do
    let
      upd (_ /\ last) = case _ of
        Add -> true /\ (last <> [ "" ])
        Delete i -> true /\ (last # fromMaybe <*> Array.deleteAt i)
        Update i v -> false /\ (last # mapWithIndex \j -> if i == j then v else _)
    envy $ memoBehFold upd (true /\ [""]) pushedRaw' \currentRaw -> do
      D.div_
        [ DC.switcherFlipped (filter fst currentRaw) \(_ /\ values) ->
            fold $ values # mapWithIndex \i value ->
              D.div_
                [ inputValidated "" "CSS selector" "" value empty
                  \newValue -> pushUpdate $ Update i newValue
                , D.button
                    ( oneOf
                        [ D.Class !:= "delete"
                        , D.OnClick !:= pushUpdate (Delete i)
                        ]
                    )
                    [ text_ "Delete" ]
                ]
        , D.button
            ( oneOf
                [ D.Class !:= "big add"
                , D.OnClick !:= pushUpdate Add
                ]
            )
            [ text_ "Add rule" ]
        , D.pre (D.Class !:= "css")
            [ DC.text $ result parser <<< snd <$> currentRaw ]
        ]
