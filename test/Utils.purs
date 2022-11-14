module Test.Utils where

import Prelude

import Data.Tuple (Tuple(..))


showTuple :: ∀ a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"
