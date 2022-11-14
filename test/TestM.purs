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
import Effect.Console as Console
import Effect.Random (randomRange)
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.RNG (class RNG)
import PartyCarlo.Data.Log as Log
import Unsafe.Coerce (unsafeCoerce)


type State = { timeCounter :: Int }

newtype TestM a = TestM ( StateT State Effect a)

runTestM :: ∀ a. TestM a -> State -> Effect a
runTestM (TestM m) s = evalStateT m s

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
        modify_ \x -> { timeCounter : x.timeCounter + 1 }
        pure <<< fromDateTime <<< forceDateTime $ adjust seconds testTime

    nowDate = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> { timeCounter : x.timeCounter + 1 }
        pure <<< date <<< forceDateTime $ adjust seconds testTime

    nowTime = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> { timeCounter : x.timeCounter + 1 }
        pure <<< time <<< forceDateTime $ adjust seconds testTime

    nowDateTime = do
        s <- get
        let seconds = Seconds (toNumber s.timeCounter)
        modify_ \x -> { timeCounter : x.timeCounter + 1 }
        pure <<< forceDateTime $ adjust seconds testTime

-- | log everything to the console
instance logMessagesTestM :: LogMessages TestM where
    logMessage log = liftEffect <<< Console.log $ Log.humanString log

-- | normal rng
instance rngTestM :: RNG TestM where
    rng = liftEffect $ randomRange 0.0 1.0
