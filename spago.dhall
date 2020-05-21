{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs" ]
, name = "my-project"
, dependencies = [ "prelude", "lists", "free" ]
, packages = ./packages.dhall
}
