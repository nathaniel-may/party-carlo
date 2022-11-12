module PartyCarlo.Pages.Home where

-- use display instead of show

import Prelude hiding (show)

import Data.Array (filter, length)
import Data.DateTime (diff)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Formatter.Number (format, Formatter(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String as String
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartyCarlo.Capability.LogMessages (class LogMessages, log)
import PartyCarlo.Capability.Now (class Now, nowDateTime)
import PartyCarlo.Components.HTML.Button (button)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Graph (graph)
import PartyCarlo.Components.HTML.Header (header)
import PartyCarlo.Components.HTML.Loading (loadingAnimation)
import PartyCarlo.Components.HTML.Utils (css)
import PartyCarlo.Data.Display (class Display, display)
import PartyCarlo.Data.Log (LogLevel(..))
import PartyCarlo.Data.Probability (Probability, p90, p95, p99, p999, probability)
import PartyCarlo.Data.Result (Interval, Result)
import PartyCarlo.Data.SortedArray as SortedArray
import PartyCarlo.Data.Tuple4 (Tuple4(..))
import PartyCarlo.MonteCarlo (confidenceInterval, sample)
import PartyCarlo.Utils (mapLeft)


data Action 
    = ReceiveInput String
    | PressButton
    | ShowBars Interval

-- State is represented as a sum type because the app is one page with different views
-- rather than multiple pages with meaninfully separate urls
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
  | Loading

data Error
    = InvalidNumber String
    | InvalidProbability String Number
    | ExperimentsFailed

-- | string used to display the error value to the user (suitable for both UI and console logs)
instance displayError :: Display Error where
  display (InvalidNumber s) = "\"" <> s <> "\"" <> " is not a number"
  display (InvalidProbability s _) = s <> " is not a probability (between 0 and 1)"
  display ExperimentsFailed = "experiments failed to run. copy your data, reload the page, and try again."

experimentCount :: Int
experimentCount = 100000

component
  :: ∀ q o m
   . MonadAff m
  => Now m
  => LogMessages m
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
      -- there's nothing to do on the loading view
      Loading -> pure unit
      -- otherwise just update the state
      Data st -> H.put (Data (st { input = s }))

  handleAction PressButton = do
    log Debug "button pressed"
    state <- H.get
    case state of
      -- there's nothing to do on the loading view
      Loading -> pure unit
      Results st -> do
        log Debug "returning to edit view"
        H.put (Data { input: st.input, e: Nothing } )
      Data st -> do
        log Debug  "run action initiated"
        case parse <<< stripInput $ st.input of
            Left e -> do
              log Debug  $ "parsing failed: " <> display e
              H.put (Data (st { e = Just e }))
            Right dist -> do
              log Debug  $ "parsed " <> (display $ length dist) <> " probabilities"
              log Info  $ "running " <> display experimentCount <> " experiments ..."
              H.put Loading
              H.liftAff <<< delay $ Milliseconds 0.0
              start <- nowDateTime
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
                      log Error  "confidence interval calculation failed"
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
                      end <- nowDateTime
                      -- TODO change show to display
                      log Info $ "result calculated in " <> display (diff end start :: Milliseconds) <> ""
                      log Debug ("result set: " <> (display t4))

  handleAction (ShowBars interval) = do
    log Debug $ fold ["showing bars for ", display interval , " inverval"]
    state <- H.get
    case state of
      -- there's nothing to do on the loading view
      Loading -> pure unit
      -- there's nothing to do on the data view
      Data _ -> pure unit
      Results st -> H.put (Results (st { result = (st.result { showBars = Just interval } ) } ) )
      
  parse :: Array String -> Either Error (Array Probability)
  parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< probability =<< parseNum s) <$> input

  parseNum :: String -> Either Error Number
  parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

  stripInput :: String -> Array String
  stripInput s = filter (not String.null) $ String.trim <$> lines s

  render :: ∀ c. State -> H.ComponentHTML Action c m
  render (Data st) =
    HH.div [ css "vcontainer" ]
      [ header
      , button "Run" PressButton
      , HH.p [ css "pcenter" ]
        [ HH.text "How many people do you expect to attend your party?" ]
      , HH.p_
        [ HH.text "Put in a probability for how likely it is for each person to attend and this will use Monte Carlo experiments to give you confidence intervals for what you think the group's attendance will be." ]
      , HH.p [ css "error" ]
        [ HH.text $ maybe "" display st.e ]
      , HH.textarea
        [ HP.id "input"
        , HP.value st.input
        , HE.onValueInput ReceiveInput
        ]
      , footer
      ]

  render Loading =
    HH.div [ css "vcontainer" ]
      [ header
      , loadingAnimation
      , footer
      ]

  render (Results st) = 
    HH.div [ css "vcontainer" ]
      [ header
      , button "Edit Data" PressButton
      , HH.h1_ [ HH.text $ display (fst st.result.p95) <> " - " <> display (snd st.result.p95) ]
      , HH.p_
        [ HH.text $ "After running " <> format commaIntFmt (toNumber experimentCount) <> " simulations of your party attendance, you are 95% confident that somewhere between " <> display (fst st.result.p95) <> " and " <> display (snd st.result.p95) <> " people will attend." ]
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
