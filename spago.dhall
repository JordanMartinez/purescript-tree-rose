{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "my-project"
, dependencies = [ "prelude", "console", "lists", "free", "spec" ]
, packages = ./packages.dhall
}
