-- | This module exports an HTML value so a consistent footer can be rendered throughout the app
module PartyCarlo.Components.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.text "PureScript + Netlify | Source on " 
    , HH.a 
        [ HP.href "https://github.com/nathaniel-may/party-carlo" ] 
        [ HH.text "GitHub" ]
    ]
