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
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "foreign"
  , "functions"
  , "integers"
  , "lcg"
  , "lists"
  , "math"
  , "mote"
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
