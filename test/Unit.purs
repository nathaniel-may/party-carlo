module Test.Unit where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.RNG (class RNG)
import PartyCarlo.Capability.Sleep (class Sleep)
import PartyCarlo.Pages.Home as Home
import Test.Capability.Assert (class Assert, assertEqual)
import Test.Capability.Metadata (class Metadata, getMeta)
import Test.TestM as TestM


allTests
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => RNG m
    => MonadState Home.State m
    => Array (m Unit)
allTests = [test0]

test0
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
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
    assertEqual "the result should have been timed, so time should have been accessed two more times than the number of logs." { actual: s.timeCounter - (length s.logs), expected: 2 }
    assertEqual "logged an unexpected number of times" { actual: length s.logs, expected: 6 }
