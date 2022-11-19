-- | This module imports all tests and runs them
module Test.Main (main) where

import Prelude

import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import PartyCarlo.Pages.Home as Home
import Test.Property as Property
import Test.TestM (runTestM, initialMeta)
import Test.Unit as Unit
import Data.Maybe (Maybe(..))


main :: Effect Unit
main = do
    -- tests in Aff
    launchAff_ do
        void $ traverse (\m -> runTestM m initialMeta (Home.Data { input : "", e : Nothing })) Unit.allTests
    -- tests in Effect
    void $ sequence Property.allTests
