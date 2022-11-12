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
  , "lists"
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
, sources =
  [ "src/**/*.purs"
  , "${if (((env:PARTYCARLO_PROD : Bool) ? False )) then ".env.prod.purs" else ".env.dev.purs"}"
  , "test/**/*.purs"
  ]
}
