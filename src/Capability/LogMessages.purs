-- | A capability representing the ability to save status information to some output, which could
-- | be the console, an external logging service, the file system or something else.
module PartyCarlo.Capability.LogMessages where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Data.Log (Log, LogLevel, mkLog)


class Monad m <= LogMessages log m where
    logMessage :: Log log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: LogMessages log m => LogMessages log (HalogenM st act slots msg m) where
    logMessage = lift <<< logMessage

-- | Log a message with a level
log :: forall m log. LogMessages log m => Now m => (log -> LogLevel) -> log -> m Unit
log f l = logMessage <=< mkLog (f l) $ l
