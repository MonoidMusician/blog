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
  [ "arrays"
  , "bifunctors"
  , "bolson"
  , "console"
  , "control"
  , "debug"
  , "deku"
  , "effect"
  , "either"
  , "enums"
  , "event"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
