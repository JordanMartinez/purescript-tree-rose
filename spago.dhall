{ sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/jordanmartinez/purescript-tree-rose"
, name = "tree-rose"
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
