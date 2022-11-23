-- | A global state for the entire application
module PartyCarlo.Store where

import Prelude

import Random.PseudoRandom (Seed)


-- | Indicates whether the app was built for dev or prod
data Env = Dev | Prod

derive instance eqLogLevel :: Eq Env
derive instance ordLogLevel :: Ord Env

type Store =
    -- what env the app was compiled for
    { env :: Env
    -- what seed the rng should use
    , seed :: Seed
    }

data Action
    = NewSeed Seed

reduce :: Store -> Action -> Store
reduce store (NewSeed seed) = store { seed = seed }
