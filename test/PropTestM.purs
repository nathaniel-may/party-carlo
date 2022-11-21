-- | simpler test monad for property tests
module Test.PropTestM where

import Prelude

import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.State.Trans (StateT, evalStateT)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import PartyCarlo.Capability.Pack (class Pack)
import Random.PseudoRandom (Seed, mkSeed)


-- TODO run in Aff instead?
newtype PropTestM a = PropTestM (StateT Seed Effect a)

runPropTestM :: ∀ a. PropTestM a -> Effect a
runPropTestM (PropTestM m) = evalStateT m (mkSeed 1)

derive newtype instance functorTestM :: Functor PropTestM
derive newtype instance applyTestM :: Apply PropTestM
derive newtype instance applicativeTestM :: Applicative PropTestM
derive newtype instance bindTestM :: Bind PropTestM
derive newtype instance monadTestM :: Monad PropTestM
derive newtype instance monadPropTestM :: MonadEffect PropTestM
derive newtype instance monadStateTestM :: MonadState Seed PropTestM

instance packSeedTestM :: Pack Seed PropTestM where
    pack = put
    unpack = get
