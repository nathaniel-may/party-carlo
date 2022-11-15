-- | This module imports all tests and runs them
module Test.Main (main) where

import Prelude

import Data.Traversable (sequence)
import Effect (Effect)
import Test.Property as Property
import Test.Unit as Unit


main :: Effect Unit
main = do
    void $ sequence Unit.allTests
    void $ sequence Property.allTests
