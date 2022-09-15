{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "bolson"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "deku"
  , "effect"
  , "either"
  , "enums"
  , "hyrule"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "free"
  , "functors"
  , "gen"
  , "integers"
  , "js-uri"
  , "lazy"
  , "lcg"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "quickcheck"
  , "record"
  , "refs"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "uri"
  , "variant"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "PureScript/src/**/*.purs" ]
}
