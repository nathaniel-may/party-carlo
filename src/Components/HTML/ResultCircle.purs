module PartyCarlo.Components.HTML.ResultCircle where

import Prelude

import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Properties as HP
import PartyCarlo.Components.HTML.Utils (css)
import PartyCarlo.Data.Display (display)
import PartyCarlo.Data.Result (Interval(..), Result)


resultCircle :: ∀ i action. Result -> Interval -> HTML i action
resultCircle result = case _ of
    P90 ->  HH.div [ css "result-circle",  HP.id "p90"]  [ HH.span_ [ HH.text $ displayRange result.p90 ] ]
    P95 ->  HH.div [ css "result-circle",  HP.id "p95"]  [ HH.span_ [ HH.text $ displayRange result.p95 ] ]
    P99 ->  HH.div [ css "result-circle",  HP.id "p99"]  [ HH.span_ [ HH.text $ displayRange result.p99 ] ]
    P999 -> HH.div [ css "result-circle",  HP.id "p999"] [ HH.span_ [ HH.text $ displayRange result.p999 ] ]

displayRange :: Tuple Int Int -> String
displayRange (Tuple low high) = display low <> " - " <> display high
