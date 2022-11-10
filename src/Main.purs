module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import PartyCarlo.Components.Root as Root


-- TODO model other effects like logging and randomization here
newtype AppM a = AppM (Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- runAppM :: forall q i o. H.Component q i o AppM -> Aff (H.Component q i o Aff)
-- runAppM (AppM x) = x

main :: Effect Unit
main = pure unit
-- main = runHalogenAff do
--   body <- awaitBody
--   runUI Root.component unit body

