{ name = "party-carlo"
, dependencies =
  [ "aff"
  , "arrays"
  , "assert"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-store"
  , "integers"
  , "lists"
  , "maybe"
  , "now"
  , "numbers"
  , "parallel"
  , "prelude"
  , "pseudo-random"
  , "quickcheck"
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
