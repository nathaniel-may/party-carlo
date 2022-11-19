-- | A capability for storing and retrieving effects as values
module Test.Capability.Metadata where

import Prelude

import Data.Tuple (Tuple(..))


class Monad m <= Metadata s m | m -> s where
    meta :: forall a. (s -> (Tuple a s)) -> m a

-- | Get the current state.
getMeta :: forall m s. Metadata s m => m s
getMeta = meta \s -> Tuple s s

-- | Get a value which depends on the current state.
getsMeta :: forall s m a. Metadata s m => (s -> a) -> m a
getsMeta f = meta \s -> Tuple (f s) s

-- | Set the state.
putMeta :: forall m s. Metadata s m => s -> m Unit
putMeta s = meta \_ -> Tuple unit s

-- | Modify the state by applying a function to the current state. The returned
-- | value is the new state value.
modifyMeta :: forall s m. Metadata s m => (s -> s) -> m s
modifyMeta f = meta \s -> let s' = f s in Tuple s' s'

modifyMeta_ :: forall s m. Metadata s m => (s -> s) -> m Unit
modifyMeta_ f = meta \s -> Tuple unit (f s)
