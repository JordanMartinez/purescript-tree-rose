{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/JordanMartinez/purescript-tree-rose"
, name = "my-project"
, dependencies =
  [ "control"
  , "foldable-traversable"
  , "free"
  , "lists"
  , "maybe"
  , "prelude"
  , "tailrec"
  ]
, packages = ./packages.dhall
}
