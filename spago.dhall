{ name = "party-carlo"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-store"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "now"
  , "numbers"
  , "prelude"
  , "quickcheck"
  , "random"
  , "safe-coerce"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
