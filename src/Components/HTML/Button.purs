module PartyCarlo.Components.HTML.Button where

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartyCarlo.Components.HTML.Utils (css)


-- | the provided action will be called on the button's click event
button :: ∀ i action. String -> action -> HH.HTML i action
button name a = HH.button
    [ css "neon"
    , HP.id "RunExperiments"
    , HP.type_ HP.ButtonButton
    , HE.onClick (\_ -> a)
    ]
    [ HH.text name ]
