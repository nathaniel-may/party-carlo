module PartyCarlo.Pages.Results where

import Prelude

import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple (fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (debug, error, log)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MonteCarlo (confidenceInterval, sample)
import PartyCarlo.Components.HTML.Button (button)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Graph (graph)
import PartyCarlo.Components.HTML.Header (header)
import PartyCarlo.Components.Utils (OpaqueSlot)
import PartyCarlo.Types (Interval, Result)
import PartyCarlo.Utils (showTuple4, Tuple4(..))
import Probability (p90, p95, p99, p999, Probability)
import SortedArray as SortedArray


type State =
  { input :: String
  , dist :: Array Probability
  , result :: Maybe Result
  }

data Action
    = RunExperiments
    | ShowBars Interval
    | EditData

type ChildSlots = (rawHtml :: OpaqueSlot Unit)

data Query a = Result a

type Input = 
    { input :: String
    , dist :: Array Probability
    }

experimentCount :: Int
experimentCount = 100000

component :: forall q o m. MonadAff m => H.Component q Input o m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }
    where
    initialState :: Input -> State
    initialState i =
        { input : i.input
        , dist : i.dist
        , result : Nothing
        }

    handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
    handleAction EditData = error "edit data not implemented"
    
    handleAction (ShowBars _) = error "show bars not implemented"

    handleAction RunExperiments = do
        debug "run action initiated"
        st <- H.get
        log $ "running " <> show experimentCount <> " experiments ..."
        start <- H.liftEffect $ map toDateTime now
        samples <- H.liftEffect $ sample st.dist experimentCount
        let sorted = SortedArray.fromArray samples
        let m4 = ( 
            Tuple4 <$> confidenceInterval p90 sorted
            <*> confidenceInterval p95 sorted
            <*> confidenceInterval p99 sorted
            <*> confidenceInterval p999 sorted)
        case m4 of
            Nothing -> do
                -- TODO this kind of eats internal errors.
                H.modify_ (_ { result = Nothing })
                error "confidence interval calculation failed"
            Just t4@(Tuple4 p90val p95val p99val p999val) -> do
                H.modify_ (_ { result = Just { dist: sorted
                                            , p90: p90val
                                            , p95: p95val
                                            , p99: p99val
                                            , p999: p999val 
                                            , showBars: Nothing } } )
                end <- H.liftEffect $ map toDateTime now
                log $ "result calculated in " <> show (diff end start :: Milliseconds) <> ":"
                debug $ "result set: " <> (showTuple4 t4)

    render :: State -> H.ComponentHTML Action ChildSlots m
    render st = HH.div
        [ HP.class_ (H.ClassName "vcontainer") ]
        [ header
        , button "Edit Data" RunExperiments
        , HH.h1_ [ HH.text case st.result of 
            Nothing -> "..."
            Just result -> show (fst result.p95) <> " - " <> show (snd result.p95)
        ]
        , HH.p [ HP.class_ (H.ClassName "pcenter") ]
        [ HH.text case st.result of 
            Nothing -> "Running " <> show experimentCount <> "experiments..."
            Just result -> "You believe with 95% confidence that somewhere between " <> show (fst result.p95) <> " and " <> show (snd result.p95) <> " people will attend."
        ]
        , graph ShowBars st.result
        , footer
        ]
