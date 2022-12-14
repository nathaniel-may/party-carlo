module PartyCarlo.Components.HTML.Loading where

import Halogen.HTML as HH
import PartyCarlo.Components.HTML.Utils (css)


-- | a pure css loading animation
loadingAnimation :: ∀ i p. HH.HTML i p
loadingAnimation = 
    HH.div [ css "loading-circle" ] 
        [ HH.div [ css "spinner" ]
            []
        ]
