-- | A capability representing the ability to get a random number.
module PartyCarlo.Capability.RNG where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

-- TODO add rngWithSeed for deterministic property testing
class Monad m <= RNG m where
    rng :: m Number

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance nowHalogenM :: RNG m => RNG (HalogenM st act slots msg m) where
    rng = lift rng
