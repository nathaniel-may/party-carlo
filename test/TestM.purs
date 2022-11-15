module Test.TestM where

import Prelude

import Control.Monad.State.Class (class MonadState, get, modify_)
import Control.Monad.State.Trans (StateT, evalStateT)
import Data.Date (exactDate)
import Data.DateTime (DateTime, adjust, date, time)
import Data.DateTime.Instant (fromDate, fromDateTime, toDateTime)
import Data.Enum (toEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomRange)
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.RNG (class RNG)
import PartyCarlo.Data.Log (Log)
import Test.Assert as Assert
import Test.Capability.Assert (class Assert)
import Unsafe.Coerce (unsafeCoerce)


type State = 
    { timeCounter :: Int 
    , logs :: Array Log
    }

initialState :: State
initialState = 
    { timeCounter : 0
    , logs : []
    }

newtype TestM a = TestM ( StateT State Effect a )

runTestM :: ∀ a. State -> TestM a -> Effect a
runTestM s (TestM m)= evalStateT m s

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance monadStateTestM :: MonadState State TestM

testTime :: DateTime
testTime = forceDateTime $ toDateTime <<< fromDate <$> do
    y <- toEnum 2022
    m <- toEnum 11
    d <- toEnum 1
    exactDate y m d

-- | so the typechecker stays sane
forceDateTime :: Maybe DateTime -> DateTime
forceDateTime = unsafeCoerce

-- | each time this is called, time progresses by exactly one second.
instance nowTestM :: Now TestM where
    now = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> ( x { timeCounter = x.timeCounter + 1 } )
        pure <<< fromDateTime <<< forceDateTime $ adjust seconds testTime

    nowDate = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> ( x { timeCounter = x.timeCounter + 1 } )
        pure <<< date <<< forceDateTime $ adjust seconds testTime

    nowTime = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> ( x { timeCounter = x.timeCounter + 1 } )
        pure <<< time <<< forceDateTime $ adjust seconds testTime

    nowDateTime = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> ( x { timeCounter = x.timeCounter + 1 } )
        pure <<< forceDateTime $ adjust seconds testTime

-- | store logs in memory in the state monad so the log statements can be tested
instance logMessagesTestM :: LogMessages TestM where
    logMessage log = modify_ \s -> (s { logs = s.logs <> [log] })

-- | normal rng
instance rngTestM :: RNG TestM where
    rng = liftEffect $ randomRange 0.0 1.0

instance assertTestM :: Assert TestM where
    assertEqual msg vals = liftEffect (Assert.assertEqual' msg vals)
