module PartyCarlo.Pages.Home where

import Prelude

import Data.Array (filter, length)
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Formatter.Number (format, Formatter(..))
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
import PartyCarlo.Components.HTML.Button (button)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Graph (graph)
import PartyCarlo.Components.HTML.Header (header)
import PartyCarlo.Components.HTML.Utils (css)
import PartyCarlo.MonteCarlo (confidenceInterval, sample)
import PartyCarlo.Types (Interval, Result)
import PartyCarlo.Utils (mapLeft, showTuple4, Tuple4(..))
import PartyCarlo.Probability (p90, p95, p99, p999, Probability, probability)
import PartyCarlo.SortedArray as SortedArray


data Action 
    = ReceiveInput String
    | PressButton
    | ShowBars Interval

-- State is either Data or Results. These could be implemented as separate pages connected by routes,
-- but instead, this is a single page with two different shapes for state.
data State
  = Data
    { input :: String
    , e :: Maybe Error
    }
  | Results
    { input :: String
    , dist :: Array Probability
    , result :: Result
    }

data Error
    = InvalidNumber String
    | InvalidProbability String Number
    | ExperimentsFailed

-- | string used to display the error value to the user (suitable for both UI and console logs)
displayError :: Error -> String
displayError (InvalidNumber s) = "\"" <> s <> "\"" <> " is not a number"
displayError (InvalidProbability s _) = s <> " is not a probability (between 0 and 1)"
displayError ExperimentsFailed = "experiments failed to run. copy your data, reload the page, and try again."

experimentCount :: Int
experimentCount = 100000

component
  :: ∀ q o m
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
    { input : ".1\n.99\n.5\n.5\n"
    , e : Nothing
    }

  handleAction :: ∀ c. Action -> H.HalogenM State Action c o m Unit
  handleAction (ReceiveInput s) = do
    state <- H.get
    case state of
      -- there's nothing to do on the results view
      Results _ -> pure unit
      -- otherwise just update the state
      Data st -> H.put (Data (st { input = s }))

  handleAction PressButton = do
    debug "button pressed"
    state <- H.get
    case state of
      Results st -> do
        debug $ "returning to edit view"
        H.put (Data { input: st.input, e: Nothing } )
      Data st -> do
        debug "run action initiated"
        case parse <<< stripInput $ st.input of
            Left e -> do
              debug $ "parsing failed: " <> displayError e
              H.put (Data (st { e = Just e }))
            Right dist -> do
              debug $ "parsed " <> (show $ length dist) <> " probabilities"
              log $ "running " <> show experimentCount <> " experiments ..."
              start <- H.liftEffect $ map toDateTime now
              samples <- H.liftEffect $ sample dist experimentCount
              let sorted = SortedArray.fromArray samples
              let m4 = ( 
                  Tuple4 <$> confidenceInterval p90 sorted
                  <*> confidenceInterval p95 sorted
                  <*> confidenceInterval p99 sorted
                  <*> confidenceInterval p999 sorted)
              case m4 of
                  Nothing -> do
                      H.put (Data ( { input: st.input, e: Just ExperimentsFailed }))
                      error "confidence interval calculation failed"
                  Just t4@(Tuple4 p90val p95val p99val p999val) -> do
                      H.put (Results (
                        { input: st.input
                        , dist: dist
                        , result: 
                          { dist: sorted
                          , p90: p90val
                          , p95: p95val
                          , p99: p99val
                          , p999: p999val 
                          , showBars: Nothing 
                          }
                        } ) )
                      end <- H.liftEffect $ map toDateTime now
                      log $ "result calculated in " <> show (diff end start :: Milliseconds) <> ":"
                      debug $ "result set: " <> (showTuple4 t4)

  handleAction (ShowBars interval) = do
    debug $ "ShowBars action called"
    state <- H.get
    case state of
      Data _ -> pure unit  -- nothing to show in the data view
      Results st -> H.put (Results (st { result = (st.result { showBars = Just interval } ) } ) )
      
  parse :: Array String -> Either Error (Array Probability)
  parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< probability =<< parseNum s) <$> input

  parseNum :: String -> Either Error Number
  parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

  stripInput :: String -> Array String
  stripInput s = filter (not String.null) $ String.trim <$> lines s

  render :: ∀ c. State -> H.ComponentHTML Action c m
  render (Data st) =
    HH.div
      [ css "vcontainer" ]
      [ header
      , button "Run" PressButton
      , HH.p [ css "pcenter" ]
        [ HH.text "How many people do you expect to attend your party?" ]
      , HH.p_
        [ HH.text "Put in a probability for how likely it is for each person to attend and this will use Monte Carlo experiments to give you confidence intervals for what you think the group's attendance will be." ]
      , HH.p [ css "error" ]
        [ HH.text $ maybe "" displayError st.e ]
      , HH.textarea
        [ HP.disabled false
        , HP.id "input"
        , HP.value st.input
        , HE.onValueInput ReceiveInput
        ]
      , footer
      ]

  render (Results st) = HH.div
    [ css "vcontainer" ]
      [ header
      , button "Edit Data" PressButton
      , HH.h1_ [ HH.text $ show (fst st.result.p95) <> " - " <> show (snd st.result.p95) ]
      , HH.p_
        [ HH.text $ "After running " <> format commaIntFmt (toNumber experimentCount) <> " simulations of your party attendance, you are 95% confident that somewhere between " <> show (fst st.result.p95) <> " and " <> show (snd st.result.p95) <> " people will attend." ]
      , HH.p_
        [ HH.text "When interpreting these results, remember that this is only a representation of what you think, which is unrelated to the liklihood of people actually showing up. Unless the input data is derived from real-world samples, these numbers cannot reflect real-world behavior." ]
      , HH.p_
        [ HH.text "The chart below is your real sample data. Explore by hovering over the boxes below for other confidence intervals from 90% to 99.9%"]
      , graph ShowBars st.result
      , footer
      ]

  commaIntFmt :: Formatter
  commaIntFmt = Formatter
    { comma: true
    , before: 0
    , after: 0
    , abbreviations: false
    , sign: false
    }
