-- | flat data shapes exposed to JS by the console batch API
module PartyCarlo.Api.Types where


type Column = { label :: String, probabilities :: Array Number }

type IntervalRow =
    { label :: String
    , p90_low :: Int
    , p90_high :: Int
    , p95_low :: Int
    , p95_high :: Int
    , p99_low :: Int
    , p99_high :: Int
    , p999_low :: Int
    , p999_high :: Int
    }
