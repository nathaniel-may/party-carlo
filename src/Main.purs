module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import PartyCarlo.Pages.Home as Home


-- TODO abstract over effects
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI Home.component unit body
