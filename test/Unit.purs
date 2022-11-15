module Test.Unit where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual')


allTests :: Array (Effect Unit)
allTests = [test0]

test0 :: Effect Unit
test0 = assertEqual' "my custom dummy test" { actual: 3, expected: 3 }
