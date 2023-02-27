{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
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
  , "lists"
  , "math"
  , "mote"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "random"
  , "spec"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
