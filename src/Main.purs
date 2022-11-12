module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import PartyCarlo.Pages.Home as Home
import PartyCarlo.ProdM (runProdM)
import PartyCarlo.Env (env)


main :: Effect Unit
main = runHalogenAff do
    body <- awaitBody
    -- the source included for the environment is determined by the sources listed in spago.dhall
    -- if the production environment variable is present, the prod value will be included.
    let initialStore = { env : env }
    root <- runProdM initialStore Home.component
    runUI root unit body
