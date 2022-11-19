module Test.Unit where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.RNG (class RNG)
import PartyCarlo.Pages.Home as Home
import Test.Capability.Assert (class Assert, assertEqual)
import Test.Capability.Metadata (class Metadata, getMeta)
import Test.TestM as TestM


allTests
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => MonadAff m
    => LogMessages m 
    => Now m
    => RNG m
    => MonadState Home.State m
    => Array (m Unit)
allTests = [test0]

test1 :: forall m. Assert m => m Unit
test1 = assertEqual "my custom dummy test" { actual: 3, expected: 3 }

test0
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => MonadAff m
    => LogMessages m 
    => Now m
    => RNG m
    => MonadState Home.State m
    => m Unit
test0 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".5" }) Home.PressButton
    s <- getMeta
    -- assert time effect and log effects happened the correct number of times
    assertEqual "time should have been called 3 times (once for log ts, twice for timing the action)" { actual: s.timeCounter, expected: 3 }
    assertEqual "two log statements should have been made" { actual: length s.logs, expected: 2 }
