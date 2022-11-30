module PartyCarlo.ProdM where

import Prelude

import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Capability.Sleep (class Sleep)
import PartyCarlo.Data.Log (LogLevel(..))
import PartyCarlo.Data.Log as Log
import PartyCarlo.Pages.Home.Logs (HomeLog)
import PartyCarlo.Store (Env(..))
import PartyCarlo.Store as Store
import PartyCarlo.Utils (randomEff)
import Safe.Coerce (coerce)


newtype ProdM a = ProdM (StoreT Store.Action Store.Store Aff a)

runProdM :: ∀ q i o. Store.Store -> H.Component q i o ProdM -> Aff (H.Component q i o Aff)
runProdM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorProdM :: Functor ProdM
derive newtype instance applyProdM :: Apply ProdM
derive newtype instance applicativeProdM :: Applicative ProdM
derive newtype instance bindProdM :: Bind ProdM
derive newtype instance monadProdM :: Monad ProdM
derive newtype instance monadEffectProdM :: MonadEffect ProdM
derive newtype instance monadAffProdM :: MonadAff ProdM
derive newtype instance monadStoreProdM :: MonadStore Store.Action Store.Store ProdM

-- | gets current time from the system
instance nowProdM :: Now ProdM where
    now = liftEffect Now.now
    nowDate = liftEffect Now.nowDate
    nowTime = liftEffect Now.nowTime
    nowDateTime = liftEffect Now.nowDateTime

-- | logs messages to the console. could add sending to a log service as well here.
instance logMessagesProdM :: LogMessages HomeLog ProdM where
    logMessage log = do
        store <- getStore
        case store.env, Log.level log of
            -- do not log debug messages in a production build
            Prod, Debug -> pure unit
            _, Debug -> logToConsole Console.debug log
            _, Info  -> logToConsole Console.info log
            _, Warn  -> logToConsole Console.warn log
            _, Error -> logToConsole Console.error log
        where
        logToConsole f log' = liftEffect <<< f $ Log.humanString log'

instance randomProdM :: Random ProdM where
    random = liftEffect randomEff

instance sleepProdM :: Sleep ProdM where
    sleep = liftAff <<< delay
