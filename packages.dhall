let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221229/packages.dhall
        sha256:a6af1091425f806ec0da34934bb6c0ab0ac1598620bbcbb60a7d463354e7d87c

in  (upstream with web-dom.version = "326c125d2f9dd5c5c1e74ac17fb95d5ddd7b4450" with language-cst-parser.version = "main" with ordered-collections.version = "v3.2.0") /\
  { free-boolean =
    { dependencies =
      [ "arrays"
      , "bifunctors"
      , "control"
      , "either"
      , "foldable-traversable"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor-lenses"
      , "record"
      , "strings"
      , "tuples"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/MonoidMusician/purescript-free-boolean.git"
    , version = "main"
    }
  , tidy =
    { dependencies =
      [ "arrays"
      , "control"
      , "dodo-printer"
      , "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "language-cst-parser"
      , "strings"
      , "tuples"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "main"
    }
  , tidy-codegen =
    { dependencies =
      [ "aff"
      , "ansi"
      , "arrays"
      , "avar"
      , "bifunctors"
      , "console"
      , "control"
      , "dodo-printer"
      , "effect"
      , "either"
      , "enums"
      , "exceptions"
      , "filterable"
      , "foldable-traversable"
      , "free"
      , "identity"
      , "integers"
      , "language-cst-parser"
      , "lazy"
      , "lists"
      , "maybe"
      , "newtype"
      , "node-buffer"
      , "node-child-process"
      , "node-fs-aff"
      , "node-path"
      , "node-process"
      , "node-streams"
      , "ordered-collections"
      , "parallel"
      , "partial"
      , "posix-types"
      , "prelude"
      , "record"
      , "safe-coerce"
      , "st"
      , "strings"
      , "tidy"
      , "transformers"
      , "tuples"
      , "type-equality"
      , "unicode"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy-codegen.git"
    , version = "main"
    }
  , stylish =
    { repo = "https://github.com/MonoidMusician/purescript-stylish.git"
    , version = "main"
    , dependencies =
      [ "console"
      , "effect"
      , "foldable-traversable"
      , "functors"
      , "identity"
      , "lists"
      , "newtype"
      , "node-process"
      , "prelude"
      , "record"
      , "transformers"
      ]
    }
  }
