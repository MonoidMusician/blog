module Conversions.Conversation where

import Prelude

import Control.Plus (empty)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Monoid.Endo (Endo(..))
import Data.Profunctor.Star (Star(..))
import Dodo (Doc)
import Dodo as Dodo
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Parser.Comb.Comber (Comber, parse)

-- X Y :: BinOp (i -> Monoid)
-- X / Y :: BinOp (i -> Maybe o)
-- "a${b}c${d}e" :: String -> o -| b, d :: String -> o
-- json: :: String -> Maybe Json
-- num: :: (String, Json) -> Maybe Number
-- .key :: Json -> Maybe Json
-- .[dynkey] :: Json -> Maybe Json -| dynkey :: Json
-- `x${y}z${u}v` :: i -> String -| y, u :: i -> String
-- :json :: Json -> String

{-
-- Indicate that we will parse JSON data which is passed through each part
JSON:
<html>
  <head>
    <meta charset="UTF-8"/>
    <style>
    ```
      /* Just write your style here */
    ```
    </style>
  </head>
  <body>
    -- the `$` indicates that we want to interpolate a string
    -- (in this case, into HTML)
    <h1>$.name</h1>
    -- It can be skipped in the case of string literals

    -- Store
    let @["recipe", .uid] := .name;

    -- This line only is used if both are present
    ( <a href=.source_url>$.source</a>
    -- If only .source is present
    / $.source
    )? -- Turn failure into mempty

    {|
    * "servings":
        -- If it ends in ` servings`, we drop that
        "${<p>$.</p>} servings"
      /
        -- To interpolate, we want to ensure the JSON is a scalar (string or number)
        -- and then HTML escape it, basically. The latter is what `$` does.
        <p>(:scalar:)</p>
    * ...
    |}?
    -- short for `scalar: . :scalar`
    -- in turn, short for `JSON:scalar . scalar:HTML`

    {|
    * "ingredients":
        <ul>
          -- Functions are called using `name()`
          -- `string:split("\n")` is just cutesy for `string: | split("\n")`
          -- `..` is `foldMap @Array`
          (string:split("\n") .. <li>$.</li>)
        </ul>
        <hr style="border: none; border-top: 1px solid pink"/>
    * "directions":
        <ol>
          -- `JSON:split("\n\n")` is similar, but if it is an array it passes it
          -- through unaltered (i.e. it assumes it is already split)
          -- `g .(`\n`). f` is `g >>> intercalateMap "\n" f`
          (JSON:split("\n\n") .(`\n`). <li>$.</li>)
        </ol>
    * ...
    |}
  </body>
</html>

let array:conjoin(string: sep, string: fail = `none`) :=
  [|
  `$fail`
  |] / [|
    * :string:
  |] / [|
    * :string:
  ` $sep `
    * :string:
  |] / [|
    -- .,.
    * ... .(`, `). :string:
  `, $sep`
    * :string:
  |]
;
-}


type M = Identity
type O = String
type Store = Map Json Json
newtype Conversation = Conversation
  (Json -> Identity { assemble :: Endo (Star Identity) Store, print :: Store -> O })

derive newtype instance semigroupConversation :: Semigroup Conversation
derive newtype instance monoidConversation :: Monoid Conversation

runConversation :: Conversation -> Json -> M O
runConversation (Conversation f) json = f json >>=
  \{ assemble: Endo (Star fromScratch), print } ->
    print <$> fromScratch Map.empty

converse :: Unit -> String -> Json -> M O
converse _ = parse grammar >>> case _ of
  Left err -> \_ -> pure err
  Right convo -> runConversation convo

grammar :: Comber Conversation
grammar = empty
