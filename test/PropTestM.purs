-- | simpler test monad for property tests
module Test.PropTestM where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Utils (randomEff)


newtype PropTestM a = PropTestM (Effect a)

runPropTestM :: ∀ a. PropTestM a -> Effect a
runPropTestM (PropTestM m) = m

derive newtype instance functorTestM :: Functor PropTestM
derive newtype instance applyTestM :: Apply PropTestM
derive newtype instance applicativeTestM :: Applicative PropTestM
derive newtype instance bindTestM :: Bind PropTestM
derive newtype instance monadTestM :: Monad PropTestM
derive newtype instance monadEffectTestM :: MonadEffect PropTestM

-- TODO put this in Aff instead with makeAff?
instance randomTestM :: Random PropTestM where
    random = liftEffect randomEff
