module PartyCarlo.Data.Log
    ( LogLevel(..)
    , Log -- no constructors exported
    , humanString
    , level
    , mkLog
    , msg
    , ts
    ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Foldable (fold)
import PartyCarlo.Capability.Now (class Now, nowDateTime)
import PartyCarlo.Data.Display (class Display, display)


data LogLevel
    = Error
    | Warn
    | Info
    | Debug

instance displayLog :: Display LogLevel where
    display Error = "ERROR"
    display Warn  = "WARN "
    display Info  = "INFO "
    display Debug = "DEBUG"

-- | simple structured log type
newtype Log = Log
    { ts :: DateTime
    , level :: LogLevel
    , msg :: String
    }

-- | Accessor for the ts field. Necessary because of the unexported constructor
ts :: Log -> DateTime
ts (Log log) = log.ts

-- | Accessor for the ts field. Necessary because of the unexported constructor
level :: Log -> LogLevel
level (Log log) = log.level

-- | Accessor for the ts field. Necessary because of the unexported constructor
msg :: Log -> String
msg (Log log) = log.msg

mkLog :: ∀ m. Now m =>  LogLevel -> String -> m Log
mkLog l m = do
    now <- nowDateTime
    pure $ Log { ts : now, level : l, msg : m }

-- | meant to be read by people as opposed to something programatically accessible like msgpack or json
humanString :: Log -> String
humanString (Log log) = fold [display log.ts, " ", display log.level, " ", log.msg]
