-- | This module will only be included if spago.dhall does not
-- | detect the production environment variable.
module PartyCarlo.Env where

import PartyCarlo.Store (Env(..))

env :: Env
env = Dev
