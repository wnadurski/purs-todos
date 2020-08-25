{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "interpolate"
  , "psci-support"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "uuid"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
