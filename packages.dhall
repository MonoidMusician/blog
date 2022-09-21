let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

let overrides =
      { js-timers =
        { dependencies = [ "refs" ]
        , repo = "https://github.com/mikesol/purescript-js-timers.git"
        , version = "rename-functions"
        }
      }

let additions =
      { hyrule =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "master"
        }
      , fast-vect =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/sigma-andex/purescript-fast-vect.git"
        , version = "main"
        }
      , bolson =
        { dependencies = [ "heterogeneous", "fast-vect" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "give-new-parent"
        }
      , variant =
        { dependencies =
          [ "assert"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "record"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/natefaubion/purescript-variant.git"
        , version = "v8.0.0"
        }
      , typelevel-eval =
        { dependencies =
          [ "effect"
          , "leibniz"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-typelevel-eval.git"
        , version = "mikesol"
        }
      , row-options =
        { dependencies = [ "homogeneous", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-row-options.git"
        , version = "v0.0.2"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      , monoid-extras =
        { dependencies = [ "profunctor-lenses" ]
        , repo = "https://github.com/mikesol/purescript-monoid-extras.git"
        , version = "v0.0.1"
        }
      , deku =
        { dependencies =
          [ "arrays"
          , "monoid-extras"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "hyrule"
          , "exists"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "indexed-monad"
          , "lists"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "record"
          , "refs"
          , "simple-json"
          , "sized-vectors"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "unsafe-coerce"
          , "variant"
          , "canvas"
          , "web-dom"
          , "web-events"
          ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "cmmnt"
        }
      }

in  upstream // overrides // additions
