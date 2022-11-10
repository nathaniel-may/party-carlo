module PartyCarlo.Pages.Home where

import Prelude

import Data.Array (filter, length)
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String as String
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (debug, error, log)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import MonteCarlo (confidenceInterval, sample)
import PartyCarlo.Components.HTML.Button (button)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Graph (graph)
import PartyCarlo.Components.HTML.Header (header)
import PartyCarlo.Components.Utils (OpaqueSlot)
import PartyCarlo.Types (Interval, Result)
import PartyCarlo.Utils (mapLeft, showTuple4, Tuple4(..))
import Probability (p90, p95, p99, p999, Probability, probability)
import SortedArray as SortedArray


data Action 
    = ReceiveInput String
    | RunExperiments
    | ShowBars Interval
    | EditData

-- | State is either Data or Results. These could be implemented as separate pages connected by routes,
-- | but instead, this is a single page with two different sets of state.
data State
  = Data
    { input :: String
    , showError :: Boolean
    , parsed :: Either Error (Array Probability)
    }
  | Results
    { input :: String
    , dist :: Array Probability
    , result :: Maybe Result
    }

type ChildSlots = (rawHtml :: OpaqueSlot Unit)

data Error
    = InvalidNumber String
    | InvalidProbability String Number

-- | string used to display the error value to the user (suitable for both UI and console logs)
displayError :: Error -> String
displayError (InvalidNumber s) = "\"" <> s <> "\"" <> " is not a number"
displayError (InvalidProbability s _) = s <> " is not a probability (between 0 and 1)"

experimentCount :: Int
experimentCount = 100000

component
  :: forall q o m
   . MonadAff m
  => H.Component q Unit o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }
  where
  initialState :: Unit -> State
  initialState _ = Data 
    { input : ""
    , showError : false
    , parsed : Right []
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction (ReceiveInput s) = do
    state <- H.get
    case state of
      -- there's nothing to do on the results view
      Results _ -> pure unit
      -- otherwise just update the state
      Data st -> H.put (Data (st { input = s }))

  handleAction RunExperiments = do
    debug "run action initiated"
    state <- H.get
    case state of
      Data st -> do
        case parse <<< stripInput $ st.input of
            Left e -> do
              debug $ "parsing failed: " <> displayError e
              H.put (Data (st { parsed = Left e }))
            Right parsed -> do
              debug $ "parsed " <> (show $ length parsed) <> " probabilities"
              -- convert the state from Data -> Results now that we have a valid distribution
              H.put (Results ( { input: st.input, dist: parsed, result: Nothing }))
              -- now that we've updated the state, recurse once to conduct the experiments
              handleAction RunExperiments
      Results st -> do
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
                H.put (Results (st { result = Nothing }))
                error "confidence interval calculation failed"
            Just t4@(Tuple4 p90val p95val p99val p999val) -> do
                H.put (Results (st { result = Just 
                  { dist: sorted
                  , p90: p90val
                  , p95: p95val
                  , p99: p99val
                  , p999: p999val 
                  , showBars: Nothing } } ) )
                end <- H.liftEffect $ map toDateTime now
                log $ "result calculated in " <> show (diff end start :: Milliseconds) <> ":"
                debug $ "result set: " <> (showTuple4 t4)

  handleAction (ShowBars _) = error "show bars not implemented"

  handleAction EditData = error "edit data not implemented"
      
  parse :: Array String -> Either Error (Array Probability)
  parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< probability =<< parseNum s) <$> input

  parseNum :: String -> Either Error Number
  parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

  stripInput :: String -> Array String
  stripInput s = filter (not String.null) $ String.trim <$> lines s

  render :: State -> H.ComponentHTML Action ChildSlots m
  render (Data st) =
    HH.div
      [ HP.class_ (H.ClassName "vcontainer") ]
      [ header
      , button "Run" RunExperiments
      , HH.p [HP.class_ (H.ClassName "pcenter")]
        [ HH.text "How many people do you expect to attend your party?" ]
      , HH.p_
        [ HH.text "Put in a probability for how likely it is for each person to attend and this will use Monte Carlo experiments to give you confidence intervals for what you think the group's attendance will be." ]
      , HH.text $ either displayError (const "") st.parsed
      , HH.textarea
        [ HP.disabled false
        , HP.id "input"
        , HP.value st.input
        , HE.onValueInput ReceiveInput
        ]
      , footer
      ]

  render (Results st) = HH.div
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
