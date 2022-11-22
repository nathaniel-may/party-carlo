module Test.Unit where

import Prelude

import Control.Monad.State.Class (class MonadState, get)
import Data.Array (length)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.Pack (class Pack)
import PartyCarlo.Capability.Sleep (class Sleep)
import PartyCarlo.Data.Probability (p95, probability)
import PartyCarlo.MonteCarlo (monteCarloConfidenceInterval)
import PartyCarlo.Pages.Home (State(..))
import PartyCarlo.Pages.Home as Home
import Random.PseudoRandom (Seed)
import Test.Capability.Assert (class Assert, assert, assertEqual, fail)
import Test.Capability.Metadata (class Metadata, getMeta)
import Test.TestM as TestM


allTests
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Pack Seed m
    => MonadState Home.State m
    => Array (m Unit)
allTests = [test0, test1, test2]

test0
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Pack Seed m
    => MonadState Home.State m
    => m Unit
test0 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".5" }) Home.PressButton
    s <- getMeta
    -- assert time effect and log effects happened the correct number of times
    assertEqual "the result should have been timed, so time should have been accessed two more times than the number of logs." { actual: s.timeCounter - (length s.logs), expected: 2 }
    assertEqual "logged an unexpected number of times" { actual: length s.logs, expected: 6 }

test1
    :: forall m
    . Assert m
    => Pack Seed m
    => m Unit
test1 = case traverse (hush <<< probability) [0.1, 0.99, 0.5, 0.5] of
    Nothing -> 
        fail "probabilies failed to parse in test1"
    Just dist ->
        monteCarloConfidenceInterval p95 Home.experimentCount dist >>= case _ of
            Nothing -> fail "monte carlo methods failed for test1"
            Just (Tuple low high) -> assert 
                "the size of the p95 confidence interval for the default input was zero" 
                (low /= high)
    
test2
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Pack Seed m
    => MonadState Home.State m
    => m Unit
test2 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".1\n.99\n.5\n.5\n" }) Home.PressButton
    s <- get
    case s of
        Results { input: _, dist: _, result: r } -> case r.p95 of
            Tuple x y -> assert "the confidence interval for the default input should not be of size zero" (x /= y)
        _ -> assert "while testing the result of pressing the button, an unexpected state change occurred." false
