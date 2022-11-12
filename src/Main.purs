module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import PartyCarlo.Pages.Home as Home
import PartyCarlo.ProdM (runProdM)
import PartyCarlo.Store (Env(..))


main :: Effect Unit
main = runHalogenAff do
    body <- awaitBody
    -- TODO fork this value at compile time based on environment variables
    let initialStore = { env : Dev }
    root <- runProdM initialStore Home.component
    runUI root unit body
