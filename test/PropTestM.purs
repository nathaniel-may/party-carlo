-- | simpler test monad for property tests
module Test.PropTestM where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomRange)
import PartyCarlo.Capability.RNG (class RNG)


newtype PropTestM a = PropTestM (Effect a)

runPropTestM :: ∀ a. PropTestM a -> Effect a
runPropTestM (PropTestM m) = m

derive newtype instance functorTestM :: Functor PropTestM
derive newtype instance applyTestM :: Apply PropTestM
derive newtype instance applicativeTestM :: Applicative PropTestM
derive newtype instance bindTestM :: Bind PropTestM
derive newtype instance monadTestM :: Monad PropTestM
derive newtype instance monadPropTestM :: MonadEffect PropTestM

-- | normal rng
instance rngTestM :: RNG PropTestM where
    rng = liftEffect $ randomRange 0.0 1.0
