module Test.TestM where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (StateT(..), runStateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Date (exactDate)
import Data.DateTime (DateTime, adjust, date, time)
import Data.DateTime.Instant (fromDate, fromDateTime, toDateTime)
import Data.Enum (toEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Capability.Sleep (class Sleep)
import PartyCarlo.Data.Log (Log)
import PartyCarlo.Pages.Home as Home
import PartyCarlo.Pages.Home.Logs (HomeLog)
import PartyCarlo.Utils (randomEff)
import Test.Assert as Assert
import Test.Capability.Assert (class Assert)
import Test.Capability.Metadata (class Metadata, getMeta, modifyMeta_)


newtype TestM a = TestM (StateT Home.State (StateT Meta Aff) a)

runTestM :: ∀ a. TestM a -> Meta -> Home.State -> Aff (Tuple a Meta)
runTestM (TestM m) meta s = runStateT (evalStateT m s) meta

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance monadAffTestM :: MonadAff TestM
derive newtype instance stateTestM :: MonadState Home.State TestM

testTime :: DateTime
testTime = forceDateTime $ toDateTime <<< fromDate <$> do
    y <- toEnum 2022
    m <- toEnum 11
    d <- toEnum 1
    exactDate y m d

-- | will infinite loop if you give it a bad date
-- | useful because all dates going through here are hard coded
forceDateTime :: Maybe DateTime -> DateTime
forceDateTime (Just x) = x
forceDateTime Nothing = forceDateTime Nothing

-- | record for storing state specific to tests
-- | intended for redirecting effects into pure values
type Meta =
    { timeCounter :: Int 
    , logs :: Array (Log HomeLog)
    }

initialMeta :: Meta
initialMeta = 
    { timeCounter : 0
    , logs : []
    }

progressTime :: TestM DateTime
progressTime = do
    meta <- getMeta
    let seconds = Seconds (toNumber meta.timeCounter)
    modifyMeta_ \s -> (s { timeCounter = s.timeCounter + 1 })
    pure <<< forceDateTime $ adjust seconds testTime

-- | each time this is called, time progresses by exactly one second.
instance nowTestM :: Now TestM where
    now = fromDateTime <$> progressTime
    nowDate = date <$> progressTime
    nowTime = time <$> progressTime
    nowDateTime = progressTime

-- | store logs in memory in the Store monad so the log statements can be tested
instance logMessagesTestM :: LogMessages HomeLog TestM where
    logMessage log = modifyMeta_ \s -> (s { logs = s.logs <> [log] })

instance randomTestM :: Random TestM where
    random = liftEffect randomEff

instance assertTestM :: Assert TestM where
    assert msg cond = liftEffect (Assert.assert' msg cond)
    assertEqual msg vals = liftEffect (Assert.assertEqual' msg vals)

instance metadataTestM :: Metadata Meta TestM where
    meta :: forall a. (Meta -> Tuple a Meta) -> TestM a
    meta f = TestM (lift (StateT $ pure <<< f))

-- | instance skips sleeping during tests
instance sleepTestM :: Sleep TestM where
    sleep _ = pure unit
