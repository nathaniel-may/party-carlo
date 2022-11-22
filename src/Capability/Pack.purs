-- | A capability for storing a value.
-- | Implemenations could be via MonadState, MonadStore, or a system call
module PartyCarlo.Capability.Pack where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)


-- TODO replace Pack Seed m with Random m?
class Monad m <= Pack v m where
    pack :: v -> m Unit
    unpack :: m v

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: Pack v m => Pack v (HalogenM st act slots msg m) where
    pack = lift <<< pack
    unpack = lift unpack
