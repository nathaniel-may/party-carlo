-- | This module imports all tests and runs them
module Test.Main (main) where

import Prelude

import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Test.Property as Property
import Test.TestM (initialState, runTestM)
import Test.Unit as Unit


main :: Effect Unit
main = do
    void $ traverse (runTestM initialState) Unit.allTests
    void $ sequence Property.allTests
