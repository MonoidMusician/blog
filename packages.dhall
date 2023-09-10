let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221229/packages.dhall
        sha256:a6af1091425f806ec0da34934bb6c0ab0ac1598620bbcbb60a7d463354e7d87c

in  upstream /\
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
  }
