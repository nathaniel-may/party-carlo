-- | This module will only be included if spago.dhall
-- | detects the production environment variable.
module PartyCarlo.Env where

import PartyCarlo.Store (Env(..))

env :: Env
env = Prod
