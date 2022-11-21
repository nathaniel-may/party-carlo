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
  -- , "lcg"
  , "lists"
  , "maybe"
  , "now"
  , "numbers"
  , "parallel"
  , "prelude"
  , "pseudo-random"
  , "quickcheck"
  , "random"
  , "safe-coerce"
  -- , "st"
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
