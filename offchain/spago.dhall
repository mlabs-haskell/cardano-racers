{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "datetime"
  , "effect"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "integers"
  , "lcg"
  , "lists"
  , "math"
  , "maybe"
  , "mote"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "record"
  , "refs"
  , "spec"
  , "strings"
  , "transformers"
  , "typelevel-prelude"
  , "uint"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs", "lib/**/*.purs" ]
}
