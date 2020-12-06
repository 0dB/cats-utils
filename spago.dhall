{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "cats"
, dependencies =
  [ "assert"
  , "console"
  , "csv"
  , "effect"
  , "flare"
  , "foreign-object"
  , "node-fs"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
