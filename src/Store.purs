-- | A global state for the entire application
module PartyCarlo.Store where

import Prelude


-- | Indicates whether the app was built for dev or prod
data Env = Dev | Prod

derive instance eqLogLevel :: Eq Env
derive instance ordLogLevel :: Ord Env

type Store =
    -- what env the app was compiled for
    { env :: Env }

type Action = Void

reduce :: Store -> Action -> Store
reduce = const
