-- | A capability for asserting within the test monad
module Test.Capability.Assert where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)


class Monad m <= Assert m where
    assert :: String -> Boolean -> m Unit
    assertEqual :: forall a. Eq a => Show a => String -> { actual :: a, expected :: a } -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions when testing a component.
instance nowHalogenM :: Assert m => Assert (HalogenM st act slots msg m) where
    assert msg cond = (assert msg cond)
    assertEqual msg vals = lift (assertEqual msg vals)

fail :: forall m. Assert m => String -> m Unit
fail = flip assert false
