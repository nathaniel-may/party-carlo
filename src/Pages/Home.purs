module PartyCarlo.Pages.Home where

-- use display instead of show

import Prelude hiding (show)

import Control.Monad.State.Class (class MonadState)
import Data.Array (filter, length)
import Data.DateTime (diff)
import Data.Either (Either(..), note)
import Data.Enum (pred, succ)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as String
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartyCarlo.Capability.LogMessages (class LogMessages, log)
import PartyCarlo.Capability.Now (class Now, nowDateTime)
import PartyCarlo.Capability.Pack (class Pack)
import PartyCarlo.Capability.Sleep (class Sleep, sleep)
import PartyCarlo.Components.HTML.Button (button)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Loading (loadingAnimation)
import PartyCarlo.Components.HTML.ResultCircle (resultCircle)
import PartyCarlo.Components.HTML.Utils (css)
import PartyCarlo.Data.Display (class Display, display)
import PartyCarlo.Data.Log (LogLevel(..))
import PartyCarlo.Data.Probability (Probability, p90, p95, p99, p999, probability)
import PartyCarlo.Data.Result (Interval(..), Result)
import PartyCarlo.Data.SortedArray as SortedArray
import PartyCarlo.MonteCarlo (confidenceInterval, sample)
import PartyCarlo.Utils (displayTrunc, mapLeft)
import Random.PseudoRandom (Seed)


data Action 
    = ReceiveInput String
    | RunExperiments
    | EditData
    | ShowBars Interval
    | ClearDefaultText
    | ResultUp
    | ResultDown

-- State is represented as a sum type because the app is one page with different views
-- rather than multiple pages with meaninfully separate urls
data State
    = Data
    { input :: String
    , e :: Maybe Error
    }
    | Results
    { input :: String
    -- TODO I think we can safely remove dist since it never gets reused.
    , dist :: Array Probability
    , result :: Result
    , show :: Interval
    }
    | Loading

-- TODO move into its own module PartyCarlo.Pages.Home.Error
data Error
    = InvalidNumber String
    | InvalidProbability String Number
    | ExperimentsFailed

derive instance eqError :: Eq Error

-- | string used to display the error value to the user (suitable for both UI and console logs)
instance displayError :: Display Error where
    display (InvalidNumber s) = "\"" <> displayTrunc 13 s <> "\"" <> " is not a number"
    display (InvalidProbability s _) = displayTrunc 13 s <> " is not a probability (between 0 and 1)"
    display ExperimentsFailed = "experiments failed to run. copy your data, reload the page, and try again."

experimentCount :: Int
experimentCount = 100000

defaultTextAreaValue :: String
defaultTextAreaValue = "Enter a list of probabilities: One for each attendee. Hit \"Run\" to get a confidence interval for overall attendance.\n\n"
    <> "ex.\n\n"
    <> ".1\n.99\n.5"

component
    :: ∀ q o m
    . MonadAff m
    => LogMessages m
    => Now m
    => Pack Seed m
    => Sleep m
    => H.Component q String o m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    } 
    where

    initialState :: String -> State
    initialState s = Data 
        { input : s
        , e : Nothing
        }

    handleAction
        :: ∀ c
        . MonadAff m
        => LogMessages m 
        => Now m
        => Pack Seed m
        => Sleep m
        => Action 
        -> H.HalogenM State Action c o m Unit
    handleAction action = do
        state <- H.get
        handleAction' state action

    render :: ∀ c. State -> H.ComponentHTML Action c m
    render (Data st) =
        HH.div [ HP.id "root", css "vcontainer" ]
        [ header
        , HH.p [ css "error" ]
            [ HH.text $ maybe " " display st.e ]
        , HH.textarea
            [ HP.id "input"
            , HP.value st.input
            , HE.onClick \_ -> ClearDefaultText
            , HE.onValueInput ReceiveInput
            ]
        , button "Run" RunExperiments
        , footer
        ]

    render Loading =
        HH.div [ HP.id "root", css "vcontainer noselect" ]
        [ header
        , loadingAnimation
        , footer
        ]

    render (Results st) = 
        HH.div [ HP.id "root", css "vcontainer" ]
        [ header
        , HH.h2_ [ HH.text $ display (fst st.result.p95) <> " - " <> display (snd st.result.p95) ]
        -- TODO move this text into an info view
        -- , HH.p_
        --     [ HH.text $ "After running " <> display experimentCount <> " simulations of your party attendance, you are 95% confident that somewhere between " <> display (fst st.result.p95) <> " and " <> display (snd st.result.p95) <> " people will attend." ]
        -- , HH.p_
        --     [ HH.text "When interpreting these results, remember that this is only a representation of what you think, which is unrelated to the liklihood of people actually showing up. Unless the input data is derived from real-world samples, these numbers cannot reflect real-world behavior." ]
        -- , HH.p_
        --     [ HH.text "The chart below is your real sample data. Explore by hovering over the boxes below for other confidence intervals from 90% to 99.9%"]
        , resultCircle st.result st.show
        , HH.div [ css "hcontainer togglerow" ]
            [ HH.div 
                [ css "toggle neon noselect"
                , HP.id "left" 
                , HE.onClick \_ -> ResultDown
                ]
                [ HH.text "←" ]
            , HH.div [ css "vcontainer showInterval" ]
                [ HH.div_ [ HH.text $ display st.show ]
                , HH.div_ [ HH.text "confidence" ] ]
            , HH.div 
                [ css "toggle neon noselect"
                , HP.id "right"
                , HE.onClick \_ -> ResultUp
                ]
                [ HH.text "→" ]
            ]
        , footer
        ]

    header :: ∀ i. HH.HTML i Action
    header = HH.h1 
        [ css "neon"
        , HE.onClick \_ -> EditData
        ] 
        [ HH.text "Party Carlo" ]

-- | a testable breakout of the handleAction function for the component
handleAction' 
    :: ∀ m
    . LogMessages m 
    => Now m
    => Pack Seed m
    => Sleep m
    => MonadState State m
    => State 
    -> Action
    -> m Unit
-- if we get input on the input page, save it to the state
handleAction' (Data st) ClearDefaultText =
    if st.input == defaultTextAreaValue
    then H.put (Data (st { input = "" }))
    else pure unit

-- no default text to clear on any other view
handleAction' _ ClearDefaultText =
    pure unit

-- view the next largest confidence interval
handleAction' (Results st) ResultUp =
    H.put (Results (st { show = fromMaybe st.show $ succ st.show }))

-- view the next smallest confidence interval
handleAction' (Results st) ResultDown =
    H.put (Results (st { show = fromMaybe st.show $ pred st.show }))

-- nothing to change outside the results view
handleAction' _ ResultUp =
    pure unit

-- nothing to change outside the results view
handleAction' _ ResultDown =
    pure unit

-- move to the edit data view from the results view
handleAction' (Results st) EditData = do
    log Debug "party carlo pressed"
    log Debug "returning to edit view"
    H.put (Data { e : Nothing, input : st.input })

-- nothing to do on other views
handleAction' _ EditData = do
    log Debug "party carlo pressed"
    pure unit

-- if we get input on the input page, save it to the state and remove any displayed error
handleAction' (Data st) (ReceiveInput s) =
    H.put (Data (st { e = Nothing, input = s }))

-- nothing to do if we recieve input on the other states
handleAction' _ (ReceiveInput _) = 
    pure unit

-- pressing the button on the data view will parse the input then run the experiments
handleAction' (Data st) RunExperiments = do
    log Debug "button pressed"
    log Debug  "run action initiated"
    case parse <<< stripInput $ st.input of
        Left e -> do
            log Debug $ "parsing failed: " <> display e
            H.put (Data (st { e = Just e }))
        Right dist -> do
            log Info  $ "parsed probabilities for " <> (display $ length dist) <> " attendees"
            log Debug $ "parsed distribution: " <> display dist
            log Info  $ "running " <> display experimentCount <> " experiments"
            H.put Loading
            sleep (Milliseconds 0.0)
            start <- nowDateTime
            exp <- runExperiments dist
            case exp of
                Left e -> do
                    log Error  "confidence interval calculation failed"
                    H.put ( Data ( { input: st.input, e: Just e } ) )
                Right r -> do
                    H.put ( Results (
                        { input: st.input
                        , dist: dist
                        , result: r
                        , show: P95
                        } ) )
                    end <- nowDateTime
                    log Info $ "result calculated in " <> display (diff end start :: Milliseconds) <> ""
                    log Debug $ fold ["result set: p90=", display r.p90, " p95=", display r.p95, " p99=", display r.p99, " p99.9=", display r.p999]

-- there's no button to run experiments on any other view
handleAction' _ RunExperiments = 
    pure unit

-- show the vertical graph bars on the results view
handleAction' (Results st) (ShowBars interval) = do
    log Debug $ fold ["showing bars for ", display interval , " inverval"]
    H.put (Results (st { result = (st.result { showBars = Just interval } ) } ) )

-- no bars to show on other views
handleAction' _ (ShowBars _) = do
    pure unit


runExperiments :: ∀ m. Pack Seed m => Array Probability -> m (Either Error Result)
runExperiments dist = do
    samples <- sample dist experimentCount
    let sorted = SortedArray.fromArray samples
    let result = (\p90val p95val p99val p999val ->
        { dist: sorted
        , p90: p90val
        , p95: p95val
        , p99: p99val
        , p999: p999val 
        , showBars: Nothing 
        }) <$> confidenceInterval  p90  sorted
            <*> confidenceInterval p95  sorted
            <*> confidenceInterval p99  sorted
            <*> confidenceInterval p999 sorted
    pure $ note ExperimentsFailed result

parse :: Array String -> Either Error (Array Probability)
parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< probability =<< parseNum s) <$> input

parseNum :: String -> Either Error Number
parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

stripInput :: String -> Array String
stripInput s = filter (not String.null) $ String.trim <$> lines s
