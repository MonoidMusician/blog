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
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Fetch (fetch)
import Foreign.Object (Object)
import Foreign.Object as Object
import Idiolect ((<#>:))
import Parser.Languages.CSS (mkCSSParser)
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones (($~~), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (hatching, inputValidated)
import Riverdragon.River (Stream, createRiver, foldStream)
import Riverdragon.River.Beyond (affToLake)
import Widget (Widget, adaptInterface)

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
  pure $ component resetting

sendExample :: Widget
sendExample { interface, attrs } = do
  let push = (interface "css-example").send
  example <- attrs "example"
  pure $ D.buttonW "" "Try this example" (push example)

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

component :: forall flow. Stream flow (Array String) -> Dragon
component resetting = hatching \shell -> do
  { stream: pushedRaw, send: pushUpdate } <- shell.track createRiver
  getParser <- shell.store $ mkCSSParser <$> affToLake fetchParser
  currentRaw <- shell.store $
    foldStream (true /\ ["",""]) (Reset <$> resetting <|> pushedRaw)
      \(_ /\ last) -> case _ of
        Add -> true /\ (last <> [ "" ])
        Delete i -> true /\ (last # fromMaybe <*> Array.deleteAt i)
        Update i v -> false /\ (last <#>: \j -> if i == j then v else _)
        Reset vs -> true /\ vs

  pure $ D.div[] $~~
    [ filter fst currentRaw >@ \(_ /\ values) ->
        Fragment $ values <#>: \i value ->
          D.div[] $~~
            [ inputValidated "terminal" "CSS selector" "" value
                ((\parser val -> if val == "" then "" else fold (blush (parser val))) <$> getParser <*> filterMap (_ !! i) (map snd currentRaw))
                \newValue -> pushUpdate $ Update i newValue
            , D.buttonW "big delete" "Delete" (pushUpdate (Delete i))
            ]
    , D.buttonW "big add" "Add selector to conjunction" (pushUpdate Add)
    , D.pre [ D.className =:= "css", D.style =:= "white-space: break-spaces;" ]
        $ Text $ result <$> getParser <*> map snd currentRaw
    ]
