-- | This module exports an HTML value so a consistent header can be rendered throughout the app
module PartyCarlo.Components.Header where

import Halogen.HTML as HH

footer :: forall i p. HH.HTML i p
footer = HH.h1_ [ HH.text "Party Carlo" ]
