module PartyCarlo.Utils where

-- anything using show here likely belongs in PartyCarlo.Data.Display instead

import Prelude hiding (show)

import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Enum (class BoundedEnum, pred, succ)
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String as String
import Data.Traversable (sequence)
import PartyCarlo.Capability.Pack (class Pack, pack, unpack)
import PartyCarlo.Data.Display (class Display, display)
import PartyCarlo.Data.Probability (Probability, probability)
import Random.PseudoRandom (Seed, randomR)


mapLeft :: ∀ a e' e. (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left <<< f) Right

note :: ∀ e a. e -> Maybe a -> Either e a
note e = maybe (Left e) Right

noteT :: ∀ e m a. MonadError e m => e -> Maybe a -> m a
noteT e = liftEither <<< note e

replicateM :: ∀ m a. Applicative m => Int -> m a -> m (Array a)
replicateM n m = sequence (Array.replicate n m)

rng :: ∀ m. Pack Seed m => m Probability
rng = do
    seed <- unpack
    let { newSeed, newVal } = randomR 0.0 1.0 seed
    pack newSeed
    -- recurse if the random number is not a valid probability
    -- (should never occur)
    either (const rng) pure (probability newVal)

rngs :: ∀ m. Pack Seed m => Int -> m (Array Probability)
rngs n = replicateM n rng

-- TODO change foldr -> foldl everywhere because PS is strict
count :: ∀ t a. Foldable t => (a -> Boolean) -> t a -> Int
count f = foldr (\x total -> if f x then total + 1 else total) 0

-- | function wrapper for if statement
if' :: ∀ a. Boolean -> a -> a -> a
if' cond x y = if cond then x else y

-- | for strings shorter than the length specified, the original is returned
-- | for longer strings, it is truncated at the desired length with "..." appended.
displayTrunc :: ∀ a. Display a => Int -> a ->  String
displayTrunc n x = if substr == s then substr else substr <> "..."
    where 
        substr = String.take n s 
        s = display x

cycleUp :: ∀ a. BoundedEnum a => a -> a
cycleUp x = fromMaybe bottom (succ x)

cycleDown :: ∀ a. BoundedEnum a => a -> a
cycleDown x = fromMaybe top (pred x)
