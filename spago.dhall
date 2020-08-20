{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-xml"
, dependencies =
  [ "console"
  , "effect"
  , "generics-rep"
  , "integers"
  , "lists"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
