module PartyCarlo.Data.Display where

import Prelude

import Data.DateTime (DateTime)
import Data.Foldable (fold)
import Data.Formatter.DateTime as FDT
import Data.Formatter.Number as FN
import Data.Int (toNumber)
import Data.List (fromFoldable)
import Data.Number.Format (toStringWith, fixed)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))


-- | A type class for user-facing string representations.
-- | Purposefully not adding the default instance for Show so that every type
-- | displayed to a user uses a format chosen by the developers.
class Display a where
    display :: a -> String

-----------------------------------------------------------------------------
-- the following instances are custom display formats for this application --
-----------------------------------------------------------------------------

-- | Strings shouldn't need to be restructured for user viewing
instance displayString :: Display String where
    display = identity

instance displayDateTime :: Display DateTime where
    display = FDT.format $ fromFoldable
        [ FDT.YearAbsolute
        , FDT.Placeholder "-"
        , FDT.MonthTwoDigits
        , FDT.Placeholder "-"
        , FDT.DayOfMonthTwoDigits
        , FDT.Placeholder " "
        , FDT.Hours24
        , FDT.Placeholder ":"
        , FDT.MinutesTwoDigits
        , FDT.Placeholder ":"
        , FDT.SecondsTwoDigits
        , FDT.Placeholder ":"
        , FDT.Milliseconds
        ]

instance displayInt :: Display Int where
    display = FN.format commaIntFmt <<< toNumber
        where 
            commaIntFmt :: FN.Formatter
            commaIntFmt = FN.Formatter
                { comma: true
                , before: 0
                , after: 0
                , abbreviations: false
                , sign: false
                }

instance displayMilliseconds :: Display Milliseconds where
    display (Milliseconds n) = toStringWith (fixed 0) n <> "ms"

instance displayTuple :: (Display a, Display b) => Display (Tuple a b) where
    display (Tuple a b) = fold ["(", display a, ",", display b, ")"]
