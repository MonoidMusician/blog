module Parser.Main.CSS where

import Prelude

import Control.Plus ((<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.BooleanAlgebra.CSS (Vert, combineFold, printVerts)
import Data.Codec.Argonaut as CA
import Data.Either (Either, blush, hush)
import Data.Filterable (filter, filterMap)
import Data.Foldable (fold, foldMap, oneOfMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Fetch (fetch)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parser.Languages.CSS (mkCSSParser)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, inputValidated)
import Riverdragon.River (Lake, createStream, foldStream)
import Riverdragon.River.Beyond (affToLake)
import Riverdragon.Test (event2Lake, host)
import Widget (Widget, adaptInterface)

type Parser = String -> Either String (Array Vert)

widgets :: Object Widget
widgets = Object.fromFoldable $
  [ "Parser.Main.CSS" /\ widgetCSS
  , "Parser.Main.CSS.Example" /\ sendExample
  ]

widgetCSS :: Widget
widgetCSS { interface, attrs } = host do
  example <- attrs "example"
  let
    initial = hush $ CA.decode (CA.array CA.string) example
    resetting = oneOfMap pure initial <|> do
      (adaptInterface (CA.array CA.string) (interface "css-example")).receive
  pure $ component $ event2Lake resetting

sendExample :: Widget
sendExample { interface, attrs } = host do
  let push = (interface "css-example").send
  example <- attrs "example"
  pure $ D.buttonN "" (push example) "Try this example"

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

component :: Lake (Array String) -> Dragon
component resetting = eggy \shell -> do
  { stream: pushedRaw, send: pushUpdate } <- shell.track createStream
  getParser <- shell.inst $ mkCSSParser <$> affToLake fetchParser
  currentRaw <- shell.inst $
    foldStream (true /\ ["",""]) (Reset <$> resetting <|> pushedRaw)
      \(_ /\ last) -> case _ of
        Add -> true /\ (last <> [ "" ])
        Delete i -> true /\ (last # fromMaybe <*> Array.deleteAt i)
        Update i v -> false /\ (last # mapWithIndex \j -> if i == j then v else _)
        Reset vs -> true /\ vs

  pure $ D.div $ Fragment
    [ Replacing $ filter fst currentRaw <#> \(_ /\ values) ->
        Fragment $ values # mapWithIndex \i value ->
          D.div $ Fragment
            [ inputValidated "terminal" "CSS selector" "" value
                ((\parser val -> if val == "" then "" else fold (blush (parser val))) <$> getParser <*> filterMap (_ !! i) (map snd currentRaw))
                \newValue -> pushUpdate $ Update i newValue
            , D.buttonN "big delete" (pushUpdate (Delete i)) "Delete"
            ]
    , D.buttonN "big add" (pushUpdate Add) "Add selector to conjunction"
    , D.pre' [ D.className "css", D.style "white-space: break-spaces;" ]
        $ Text $ result <$> getParser <*> map snd currentRaw
    ]
