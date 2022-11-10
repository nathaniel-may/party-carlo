module PartyCarlo.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Number ((%))


data Tuple4 a b c d = Tuple4 a b c d

showTuple :: ∀ a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"

showTuple4 :: ∀ a b c d. Show a => Show b => Show c => Show d => Tuple4 a b c d -> String
showTuple4 (Tuple4 a b c d) = "(" <> show a <> ", " <> show b <> ", " <> show c <> ", " <> show d <> ")"

probErr :: Number -> String
probErr n = "error parsing distribution probabilities: " <> show n <> " is not between 0.0 and 1.0"

mapLeft :: ∀ a e' e. (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left <<< f) Right

note :: ∀ e a. e -> Maybe a -> Either e a
note e = maybe (Left e) Right

noteT :: ∀ e m a. MonadError e m => e -> Maybe a -> m a
noteT e = liftEither <<< note e
