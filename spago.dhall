{ name = "phylio"
, license = "MIT"
, repository = "https://github.com/vibbits/phylio"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "enums"
  , "filterable"
  , "foldable-traversable"
  , "graphs"
  , "identity"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
