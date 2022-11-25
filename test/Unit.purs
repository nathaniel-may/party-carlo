-- | module for all unit tests
module Test.Unit
    -- exporting only the full array to get dead code warnings if written tests aren't in the array
    (allTests) 
    where

import Prelude

import Control.Monad.State.Class (class MonadState, get)
import Data.Array (length)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Capability.Sleep (class Sleep)
import PartyCarlo.Data.Probability (p95, mkProbability)
import PartyCarlo.MonteCarlo (monteCarloConfidenceInterval)
import PartyCarlo.Pages.Home (Error(..), State(..))
import PartyCarlo.Pages.Home as Home
import Test.Capability.Assert (class Assert, assert, assertEqual, fail)
import Test.Capability.Metadata (class Metadata, getMeta)
import Test.TestM as TestM


-- | array of all tests to run
allTests
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Random m
    => MonadState Home.State m
    => Array (m Unit)
allTests = [test0, test1, test2, test3, test4]

-- | test that time and logging effects are taking place a reasonable amount of times within the component's handleAction function
test0
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test0 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".5" }) Home.ButtonPress
    s <- getMeta
    -- assert time effect and log effects happened the correct number of times
    assertEqual "the result should have been timed, so time should have been accessed two more times than the number of logs." { actual: s.timeCounter - (length s.logs), expected: 2 }
    assert "logged less than expected" (length s.logs >= 6)

-- | test that the monte carlo library returns confidence intervals of size > 0.
-- | this is useful for catching problems with the rng or seed storage
test1
    :: forall m
    . Assert m
    => Random m
    => m Unit
test1 = case traverse (hush <<< mkProbability) [0.1, 0.99, 0.5, 0.5] of
    Nothing -> 
        fail "probabilies failed to parse in test1"
    Just dist ->
        monteCarloConfidenceInterval p95 Home.experimentCount dist >>= case _ of
            Nothing -> fail "monte carlo methods failed for test1"
            Just (Tuple low high) -> assert 
                "the size of the p95 confidence interval for the default input was zero" 
                (low /= high)

-- | test that the monte carlo library returns confidence intervals of size > 0 but within the handleAction function.
-- | this is useful for catching problems with the rng or seed storage
test2
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test2 = do
    -- press the button with well formed input
    Home.handleAction' (Home.Data { e : Nothing, input : ".1\n.99\n.5\n.5\n" }) Home.ButtonPress
    s <- get
    case s of
        Results { input: _, dist: _, result: r } -> case r.p95 of
            Tuple x y -> assert "the confidence interval for the default input should not be of size zero" (x /= y)
        _ -> assert "while testing the result of pressing the button, an unexpected state change occurred." false

-- | test that bad input is handled properly when the button is pressed the component's handleAction function
test3
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test3 = do
    let badInput = "bad input"
    Home.handleAction' (Home.Data { e : Nothing, input : badInput }) Home.ButtonPress
    get >>= case _ of
        Data state ->
            assert "pressing the button with a bad non-Number input should yeild an InvalidNumber error" (state.e == Just (InvalidNumber badInput))
        Loading ->
            fail $ "pressing the button with a bad input yielded the invalid state `Loading` in test 3"
        Results _ ->
            fail $ "pressing the button with a bad input yielded the invalid state `Results` in test 3"
    
-- | test that bad input is handled properly when the button is pressed the component's handleAction function
test4
    :: forall m
    . Assert m
    => Metadata TestM.Meta m
    => Sleep m
    => LogMessages m 
    => Now m
    => Random m
    => MonadState Home.State m
    => m Unit
test4 = do
    let badInput = "2"
    Home.handleAction' (Home.Data { e : Nothing, input : badInput }) Home.ButtonPress
    get >>= case _ of
        Data state ->
            assert "pressing the button with a bad Number input should yeild an InvalidProbability error" (state.e == Just (InvalidProbability badInput 2.0))
        Loading ->
            fail $ "pressing the button with a bad input yielded the invalid state `Loading` in test 4"
        Results _ ->
            fail $ "pressing the button with a bad input yielded the invalid state `Results` in test 4"
    