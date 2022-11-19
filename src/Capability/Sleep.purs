-- | A capability for pausing execution
module PartyCarlo.Capability.Sleep where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Time.Duration (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)


class MonadAff m <= Sleep m where
    sleep :: Milliseconds -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance nowHalogenM :: Sleep m => Sleep (HalogenM st act slots msg m) where
    sleep = lift <<< sleep
