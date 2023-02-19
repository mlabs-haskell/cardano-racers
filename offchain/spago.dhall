{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "exceptions"
  , "mote"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "spec"
  , "uint"
  , "integers"
  , "aeson"
  , "partial"
  , "bifunctors"
  , "foreign-object"
  , "control"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
