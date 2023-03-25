{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "control"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foreign-object"
  , "integers"
  , "lcg"
  , "lists"
  , "math"
  , "mote"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "refs"
  , "strings"
  , "spec"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs", "demos/**/*.purs" ]
}
