{ name = "party-carlo"
, dependencies =
  [ "aff"
  , "arrays"
  , "assert"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
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
  , "parallel"
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
  , "${if    env:PARTYCARLO_PROD ? False
       then  ".env.prod.purs"
       else  ".env.dev.purs"}"
  , "test/**/*.purs"
  ]
}
