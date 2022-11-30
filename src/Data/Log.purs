module PartyCarlo.Data.Log
    ( LogLevel(..)
    , Log -- no constructors exported
    , humanString
    , level
    , mkLog
    , msg
    , ts
    , vals
    ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import PartyCarlo.Capability.Now (class Now, nowDateTime)
import PartyCarlo.Data.Display (class Display, display)


data LogLevel
    = Error
    | Warn
    | Info
    | Debug

derive instance eqLogLevel :: Eq LogLevel
derive instance genericLogLevel :: Generic LogLevel _

instance showLogLevel :: Show LogLevel where
    show = genericShow

instance displayLogLevel :: Display LogLevel where
    display Error = "ERROR"
    display Warn  = "WARN "
    display Info  = "INFO "
    display Debug = "DEBUG"

-- | simple structured log type
data Log a = Log
    { ts :: DateTime
    , level :: LogLevel
    , vals :: a
    }

instance showLog :: Show a => Show (Log a) where
    show (Log x) = "(Log " <> show x <> ")" 

-- | Accessor for the ts field. Necessary because of the unexported constructor
ts :: forall a. Log a -> DateTime
ts (Log log) = log.ts

-- | Accessor for the ts field. Necessary because of the unexported constructor
level :: forall a. Log a -> LogLevel
level (Log log) = log.level

vals :: forall a. Log a -> a
vals (Log log) = log.vals

-- | Generates the message from the structured values.
msg :: forall a. Display a => Log a -> String
msg (Log log) = display log.vals

mkLog :: ∀ m a. Now m => LogLevel -> a -> m (Log a)
mkLog l x = do
    now <- nowDateTime
    pure $ Log { ts : now, level : l, vals : x }

-- | meant to be read by people as opposed to something programatically accessible like msgpack or json
humanString :: forall a. Display a => Log a -> String
humanString l@(Log log) = fold [display log.ts, " ", display log.level, " ", msg l]
