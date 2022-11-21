let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

in  upstream
  with pseudo-random =
    { dependencies =
        [ "console"
        , "effect"
        , "lcg"
        , "arrays"
        , "st"
        ]
    , repo =
        "https://github.com/opyapeus/purescript-pseudo-random.git"
    , version =
        "v0.2.2"
    }
