{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "todos-purs"
, dependencies =
  [ "console", "effect", "psci-support", "halogen", "coroutines" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
