module Test.Unit where

import Prelude

import Test.Capability.Assert (class Assert, assertEqual)


allTests :: forall m. Assert m => Array (m Unit)
allTests = [test0]

test0 :: forall m. Assert m => m Unit
test0 = assertEqual "my custom dummy test" { actual: 3, expected: 3 }
