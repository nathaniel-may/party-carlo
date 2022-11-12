-- | A capability representing the ability to save status information to some output, which could
-- | be the console, an external logging service, the file system or something else.
module PartyCarlo.Capability.LogMessages where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Data.Log (LogLevel, Log, mkLog)


class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

-- | Log a message with a level
log :: forall m. LogMessages m => Now m => LogLevel -> String -> m Unit
log reason = logMessage <=< mkLog reason
