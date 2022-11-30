-- | A capability representing the ability to save status information to some output, which could
-- | be the console, an external logging service, the file system or something else.
module PartyCarlo.Capability.LogMessages where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import PartyCarlo.Data.Log (Log)


class Monad m <= LogMessages log m where
    logMessage :: Log log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: LogMessages log m => LogMessages log (HalogenM st act slots msg m) where
    logMessage = lift <<< logMessage
