-- | A global state for the entire application
module PartyCarlo.Store where

import Prelude


-- | Indicates whether we are running in dev or prod
data Env = Dev | Prod

derive instance eqLogLevel :: Eq Env
derive instance ordLogLevel :: Ord Env

-- | Right now the store only has the environment but this could be extended to many more fields
type Store = { env :: Env }

-- | Dummy concrete action necessary for deriving typeclasses
type Action = Void

-- | the store cannot change so the reducer is trivial
reduce :: Store -> Action -> Store
reduce = const
