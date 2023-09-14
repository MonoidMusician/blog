module Parser.Main.CSS where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.BooleanAlgebra.CSS (Vert, combineFold, printVerts, subsumptite)
import Data.Codec.Argonaut as CA
import Data.Either (Either, blush, hush)
import Data.Filterable (filter, filterMap)
import Data.Foldable (fold, foldMap, oneOf, oneOfMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra (tt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=))
import Deku.Control (text_)
import Deku.Control as DC
import Deku.Core (Domable, bussed, envy)
import Deku.DOM as D
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Console (log)
import FRP.Event (Event, makeEvent, memoize)
import FRP.Memoize (memoBehFold, memoLast)
import Fetch (fetch)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parser.Languages.CSS (mkCSSParser)
import Parser.Main (inputValidated)
import Unsafe.Coerce (unsafeCoerce)
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

type Parser = String -> Either String (Array Vert)

widgets :: Object Widget
widgets = Object.fromFoldable $
  [ "Parser.Main.CSS" /\ widgetCSS
  , "Parser.Main.CSS.Example" /\ sendExample
  ]

widgetCSS :: Widget
widgetCSS { interface, attrs } = do
  example <- attrs "example"
  let
    initial = hush $ CA.decode (CA.array CA.string) example
    resetting = oneOfMap pure initial <|> do
      (adaptInterface (CA.array CA.string) (interface "css-example")).receive
  pure $ SafeNut do
    component resetting

affToEvent :: forall a. Aff a -> Event (Maybe a)
affToEvent aff = makeEvent \cb -> do
  fiber <- Aff.runAff (cb <<< hush) aff
  pure $ Aff.launchAff_ $ Aff.killFiber (Aff.error "event unsubscribed") fiber

sendExample :: Widget
sendExample { interface, attrs } = do
  let push = (interface "css-example").send
  example <- attrs "example"
  pure $ SafeNut do
    D.button
      ( oneOf
          [ D.Class !:= ""
          , D.OnClick !:= push example
          ]
      )
      [ text_ "Try this example" ]

result :: Parser -> Array String -> String
result parser =
  filterMap (parser >>> hush)
    >>> sequence
    >>> foldMap combineFold
    >>> printVerts

data Update
  = Update Int String
  | Delete Int
  | Add
  | Reset (Array String)

fetchParser :: Aff String
fetchParser = _.text =<< fetch "assets/json/css-parser-states.json" {}

component :: forall lock payload. Event (Array String) -> Domable lock payload
component resetting =
  bussed \pushUpdate pushedRaw -> do
    envy $ memoLast (mkCSSParser <$> affToEvent fetchParser) \getParser -> do
      let
        upd (_ /\ last) = case _ of
          Add -> true /\ (last <> [ "" ])
          Delete i -> true /\ (last # fromMaybe <*> Array.deleteAt i)
          Update i v -> false /\ (last # mapWithIndex \j -> if i == j then v else _)
          Reset vs -> true /\ vs
      envy $ memoBehFold upd (true /\ ["",""]) (Reset <$> resetting <|> pushedRaw) \currentRaw -> do
        D.div_
          [ DC.switcherFlipped (filter fst currentRaw) \(_ /\ values) ->
              fold $ values # mapWithIndex \i value ->
                D.div_
                  [ inputValidated "terminal" "CSS selector" "" value
                    ((\parser val -> if val == "" then "" else fold (blush (parser val))) <$> getParser <*> filterMap (_ !! i) (map snd currentRaw))
                    \newValue -> pushUpdate $ Update i newValue
                  , D.button
                      ( oneOf
                          [ D.Class !:= "big delete"
                          , D.OnClick !:= pushUpdate (Delete i)
                          ]
                      )
                      [ text_ "Delete" ]
                  , D.br__ ""
                  , D.br__ ""
                  ]
          , D.button
              ( oneOf
                  [ D.Class !:= "big add"
                  , D.OnClick !:= pushUpdate Add
                  ]
              )
              [ text_ "Add selector to conjunction" ]
          , D.pre (D.Class !:= "css" <|> D.Style !:= "text-wrap: wrap")
              [ DC.text $ result <$> getParser <*> map snd currentRaw ]
          ]
