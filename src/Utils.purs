module PartyCarlo.Utils where

-- anything using show here likely belongs in PartyCarlo.Data.Display instead

import Prelude hiding (show)

import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)


mapLeft :: ∀ a e' e. (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left <<< f) Right

note :: ∀ e a. e -> Maybe a -> Either e a
note e = maybe (Left e) Right

noteT :: ∀ e m a. MonadError e m => e -> Maybe a -> m a
noteT e = liftEither <<< note e

replicateM :: ∀ m a. Applicative m => Int -> m a -> m (Array a)
replicateM n m = sequence (Array.replicate n m)
