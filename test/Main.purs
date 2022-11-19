-- | This module imports all tests and runs them
module Test.Main (main) where

import Prelude

import Control.Parallel (parTraverse)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import PartyCarlo.Pages.Home as Home
import Test.Property as Property
import Test.TestM (runTestM, initialMeta)
import Test.Unit as Unit


main :: Effect Unit
main = do
    -- tests in Aff
    launchAff_ do
        void $ parTraverse (\m -> runTestM m initialMeta (Home.Data { input : "", e : Nothing })) Unit.allTests
    -- tests in Effect
    void $ sequence Property.allTests
