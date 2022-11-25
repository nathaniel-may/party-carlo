-- | A capability for generating a random number effectfully
-- TODO swap this back out for a pure prng if I can find one that's fast enough
module PartyCarlo.Capability.Random where

import Control.Monad.Trans.Class (lift)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)
import PartyCarlo.Data.Probability (Probability)


class MonadEffect m <= Random m where
    random :: m Probability

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: Random m => Random (HalogenM st act slots msg m) where
    random = lift random
