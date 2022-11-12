module PartyCarlo.Data.Display where

import Prelude

import Data.DateTime (DateTime)


-- | A type class for user-facing string representations.
-- | Purposefully not adding the default instance for Show so that every type
-- | displayed to a user uses a format chosen by the developers.
class Display a where
    display :: a -> String

-- | Strings shouldn't need to be restructured for user viewing
instance displayString :: Display String where
    display = identity

instance displayDateTime :: Display DateTime where
    display = show
