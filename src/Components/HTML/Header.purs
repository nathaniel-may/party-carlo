-- | This module exports an HTML value so a consistent header can be rendered throughout the app
module PartyCarlo.Components.HTML.Header where

import Halogen.HTML as HH

header :: ∀ i p. HH.HTML i p
header = HH.h1_ [ HH.text "Party Carlo" ]
