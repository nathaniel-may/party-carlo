-- | simpler test monad for property tests
module Test.PropTestM where

import Prelude

import Control.Monad.State (State, evalState)
import Control.Monad.State.Class (class MonadState, get, put)
import PartyCarlo.Capability.Pack (class Pack)
import Random.PseudoRandom (Seed)


newtype PropTestM a = PropTestM (State Seed a)

runPropTestM :: ∀ a. PropTestM a -> Seed -> a
runPropTestM (PropTestM m) seed = evalState m seed

derive newtype instance functorTestM :: Functor PropTestM
derive newtype instance applyTestM :: Apply PropTestM
derive newtype instance applicativeTestM :: Applicative PropTestM
derive newtype instance bindTestM :: Bind PropTestM
derive newtype instance monadTestM :: Monad PropTestM
derive newtype instance monadStateTestM :: MonadState Seed PropTestM

instance packSeedTestM :: Pack Seed PropTestM where
    pack = put
    unpack = get
