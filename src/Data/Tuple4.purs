module PartyCarlo.Data.Tuple4 where

import Data.Foldable (fold)
import PartyCarlo.Data.Display (class Display, display)


data Tuple4 a b c d = Tuple4 a b c d

instance displayTuple4 :: (Display a, Display b, Display c, Display d) => Display (Tuple4 a b c d) where
    display (Tuple4 a b c d) = fold ["(", display a, ",", display b, ",", display c, ",", display d, ")"]
