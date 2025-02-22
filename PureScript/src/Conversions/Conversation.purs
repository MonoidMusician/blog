module Conversions.Conversation where

import Parser.Parserlude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.String.Regex (regex, source, flags, test, match, replace, replace', search, split) as Re
import Data.String.Regex.Flags (dotAll, global, ignoreCase, multiline, noFlags, sticky, unicode) as Re
import Data.String.Regex.Unsafe (unsafeRegex) as Re
import Dodo (Doc)
import Dodo as T
import Dodo as Dodo
import Dodo.Common as T
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Parser.Comb.Comber (choices)

-- X Y :: BinOp (i -> Monoid)
-- X / Y :: BinOp (i -> Maybe o)
-- "a${b}c${d}e" :: String -> o -| b, d :: String -> o
-- json: :: String -> Maybe Json
-- num: :: (String, Json) -> Maybe Number
-- .key :: Json -> Maybe Json
-- .[dynkey] :: Json -> Maybe Json -| dynkey :: Scalar
-- `x${y}z${u}v` :: i -> String -| y, u :: i -> String
-- :json :: Json -> String


-- "\n":string:CSS = "\\n":CSS

{-
-- Indicate that we will parse JSON data which is passed through each part
JSON:
<html>
  <head>
    <meta charset="UTF-8"/>
    <style>
    """
      /* Just write your style here */
      /* (Triple quotes for HTML escaping) */
    """
    </style>
  </head>
  <body>
    -- We can pull out individual fields and render them to HTML
    <h1>.name :string</h1>
    -- `:text` and `:string` are different:
    --   `:string` is an assertion
    --   `:text` will render JSON, etc.

    -- Store
    let @["recipe", .uid] := .name;

    -- This line only is used if both are present
    ( <a href=.source_url>.source</a>
    -- If only .source is present
    / .source
    )? -- Turn failure into mempty
    -- `_?` binds tighter than `_/_`

    {|
    * `servings`:
        -- If it ends in ` servings`, we drop that and output a paragraph
        -- (with whitespace on both sides dropped)
        parser: /\s*/! <p>.</p> ` servings` /\s*/!
      /
        -- To interpolate, we want to ensure the JSON is a scalar
        -- (string or number) and then HTML escape it, basically.
        <p>:scalar:</p>
        -- short for `scalar: . :scalar`
        -- in turn, short for `JSON:scalar . scalar:HTML`
    ...!
      -- match remaining fields (otherwise it requires
      -- an exact match with no extra fields),
      -- but cancel any output with `!`
    |}?

    {|
    * `ingredients`:
        <ul>
          -- Functions are called using `name()`
          -- `string:split("\n")` is just cutesy for `string: split("\n")`
          -- (they are namespaced by type anyways)
          -- `..` is `foldMap @Array`
          string:split("\n") .. <li>.</li>
        </ul>
        <hr style="border: none; border-top: 1px solid pink"/>
    * `directions`:
        <ol>
          -- `JSON:split("\n\n")` is similar, but if it is an array it passes it
          -- through unaltered (i.e. it assumes it is already split)
          -- `g .("\n"). f` is `g >>> intercalateMap "\n" f`
          JSON:split("\n\n") .("\n"). <li>.</li>
        </ol>
    ...!
    |} -- these two are mandatory,
       -- it's not a usable recipe without them
  </body>
</html>

let array:conjoin(string: sep, string: fail = "none"):string :=
  [|
  -- If the array is empty, output the fail string
  -- as saved in the variable `fail`
  "$fail"
  |] / [|
  -- Singleton array: just output the one element
    * :string:
  |] / [|
  -- Doubleton array: output the two with the separator
    * :string:
  " $sep "
    * :string:
  |] / [|
  -- If it is tripleton or more: take the first items,
  -- comma separated
    -- .,.
    ... .(", "). :string:
  ", $sep "
  -- And output the final element
    * :string:
  |]
;
-}

operators :: Comber Unit -> Comber Unit
operators extension =
  "operators_alt" #-> \opAlt -> opInfixR (token "/") opAlt $
  "operators_pipe" #-> \opPipe -> opInfixR (token "|") opPipe $
  "operators_adj" #-> \opAdj -> opInfixR (pure "") opAdj $
  "operators_opt" #-> \opOpt -> opPostfix (token "?" <|> token "!") opOpt $
  extension
  where
  opInfixR sep v' v = (v <* sep <* v') <|> v
  opPostfix sep v' v = v <|> v' <* sep

iJSON_or :: Comber Unit -> Comber Unit
iJSON_or extension = "iJSON" #-> \iJSON -> choices
  [ "iJSON_array" #: delim "[|" "|]" do
      choices
        [ token "..." *> iJSON
        , token "*" *> iJSON
        ]
  , extension
  ]

oJSON_or :: Comber Unit -> Comber Unit
oJSON_or extension = "oJSON" #-> \oJSON -> choices
  [ delim "[" "]" $ void $ manyBaseSepBy "oJSON_array" ws (token ",") oJSON
  , extension
  ]

oHTML_or :: Comber Unit -> Comber Unit
oHTML_or extension = "oHTML" #-> \oHTML -> choices
  [ "oHTML_open_close" #: ado
      token "<"
      name <- rawr "\\w+"
      -- attributes
      token "/"
      token ">"
      children <- oHTML
      token "<"
      token "/"
      name2 <- rawr "\\w+"
      token ">"
      in unit
  , "oHTML_single" #: ado
      token "<"
      name <- rawr "\\w+"
      -- attributes
      token "/"
      token ">"
      in unit
  , extension
  ]

iParser :: Comber Unit -> Comber Unit
iParser extension = choices
  [ delim "/" "/" $ empty
  , delim "`" "`" $ empty
  , extension
  ]


data HTML
  = Fragment (Array HTML)
  | Text String
  | Raw String
  | Element
    { name :: String
    , attrs :: Array
      { name :: String
      , value :: String
      }
    , content :: HTML
    }

instance Monoid HTML where
  mempty = Fragment []
instance Semigroup HTML where
  append (Fragment []) vdom = vdom
  append vdom (Fragment []) = vdom
  append (Fragment v1s) (Fragment v2s) = Fragment (v1s <> v2s)

  -- Keep flattening, to keep it associative
  append v1 (Fragment v2s) = Fragment ([v1] <> v2s)
  append (Fragment v1s) v2 = Fragment (v1s <> [v2])

  -- Finally, if none of the above cases apply,
  -- we wrap it up in a two-element array:
  append v1 v2 = Fragment [v1, v2]


newtype NDJSON = NDJSON (Array Json)

derive instance Newtype NDJSON _
derive newtype instance Semigroup NDJSON
derive newtype instance Monoid NDJSON

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
