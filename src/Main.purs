module Main where

import Prelude

import Data.DateTime (time)
import Data.Enum (fromEnum)
import Data.Time (millisecond)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import PartyCarlo.Env (env)
import PartyCarlo.Pages.Home as Home
import PartyCarlo.ProdM (runProdM)
import Random.PseudoRandom (mkSeed)


main :: Effect Unit
main = runHalogenAff do
    body <- awaitBody
    -- get the system time to seed the rng with the millis component.
    -- It's only 1000 possible seeds, but that's plenty.
    dt <- liftEffect nowDateTime
    let seed = mkSeed <<< fromEnum <<< millisecond $ time dt
    -- the source included for the environment is determined by the sources listed in spago.dhall
    -- if the production environment variable is present, the prod value will be included.
    let initialStore = { env : env, seed : seed }
    root <- runProdM initialStore Home.component
    runUI root initialInput body

initialInput :: String
initialInput = ".1\n.99\n.5\n.5\n"
