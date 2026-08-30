module PartyCarlo.Pages.Home where

-- use display instead of show

import Prelude hiding (show)

import Control.Monad.State.Class (class MonadState)
import Data.Either (Either(..))
import Data.Enum (pred, succ)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartyCarlo.Capability.LogMessages (class LogMessages)
import PartyCarlo.Capability.Now (class Now, nowDateTime)
import PartyCarlo.Capability.Random (class Random)
import PartyCarlo.Capability.Sleep (class Sleep, sleep)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Loading (loadingAnimation)
import PartyCarlo.Components.HTML.ResultCircle (resultCircle)
import PartyCarlo.Components.HTML.Utils (css)
import PartyCarlo.Core (experimentCount, parse, runExperiments, stripInput)
import PartyCarlo.Core.Error (Error)
import PartyCarlo.Data.Display (display)
import PartyCarlo.Data.Result (Interval(..), Result)
import PartyCarlo.Pages.Home.Logs (HomeLog(..), HomeStateType(..), log)


data Action 
    = ReceiveInput String
    | ButtonPress
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
    , result :: Result
    , show :: Interval
    }
    | Loading

defaultTextAreaValue :: String
defaultTextAreaValue = "Enter a list of probabilities: One for each attendee.\n\n"
    <> "Use the button below to get a confidence interval for overall attendance.\n\n"
    <> "ex.\n\n"
    <> ".1\n.99\n.5"

component
    :: ∀ q o m
    . MonadAff m
    => LogMessages HomeLog m
    => Now m
    => Random m
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
        => LogMessages HomeLog m 
        => Now m
        => Random m
        => Sleep m
        => Action 
        -> H.HalogenM State Action c o m Unit
    handleAction action = do
        state <- H.get
        handleAction' state action

    render :: ∀ c. State -> H.ComponentHTML Action c m
    render (Data st) =
        HH.div [ HP.id "root", css "vcontainer data-view" ]
        [ title
        , HH.p [ css "error" ]
            [ HH.text $ maybe " " display st.e ]
        , HH.textarea
            [ HP.id "input"
            , HP.value st.input
            , HE.onClick \_ -> ClearDefaultText
            , HE.onValueInput ReceiveInput
            ]
        , HH.button
            [ css "submit neon noselect"
            , HE.onClick \_ -> ButtonPress
            ]
            [ HH.text "Get the numbers" ]
        , footer
        ]

    render Loading =
        HH.div [ HP.id "root", css "vcontainer noselect" ]
        [ title
        , loadingAnimation
        , renderToggleRow Loading
        , footer
        ]

    render (Results st) = 
        HH.div [ HP.id "root", css "vcontainer" ]
        [ title
        -- TODO move this text into an info view
        -- , HH.p_
        --     [ HH.text $ "After running " <> display experimentCount <> " simulations of your party attendance, you are 95% confident that somewhere between " <> display (fst st.result.p95) <> " and " <> display (snd st.result.p95) <> " people will attend." ]
        -- , HH.p_
        --     [ HH.text "When interpreting these results, remember that this is only a representation of what you think, which is unrelated to the liklihood of people actually showing up. Unless the input data is derived from real-world samples, these numbers cannot reflect real-world behavior." ]
        -- , HH.p_
        --     [ HH.text "The chart below is your real sample data. Explore by hovering over the boxes below for other confidence intervals from 90% to 99.9%"]
        , resultCircle st.result st.show
        , renderToggleRow (Results st)
        , footer
        ]

    title :: ∀ i. HH.HTML i Action
    title = HH.h1
        [ css "title neon noselect" ]
        [ HH.text "Party Carlo" ]

    renderToggleRow :: ∀ i. State -> HH.HTML i Action
    renderToggleRow state = 
        let 
            showInterval = case state of
                Results st -> 
                    [ HH.div_ [ HH.text $ display st.show ]
                    , HH.div_ [ HH.text "confidence" ] ]
                _ ->
                    [ HH.div_ [ HH.text "" ]
                    , HH.div_ [ HH.text "" ] ]
        in 
            HH.div [ css "hcontainer togglerow" ]
                [ HH.div 
                    [ css "toggle neon noselect"
                    , HP.id "left" 
                    , HE.onClick \_ -> ResultDown
                    ]
                    [ HH.text "←" ]
                , HH.div [ css "vcontainer show-interval" ]
                    showInterval
                , HH.div 
                    [ css "toggle neon noselect"
                    , HP.id "right"
                    , HE.onClick \_ -> ResultUp
                    ]
                    [ HH.text "→" ]
                ]


-- | a testable breakout of the handleAction function for the component
handleAction' 
    :: ∀ m
    . LogMessages HomeLog m 
    => Now m
    => Random m
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

-- if we get input on the input page, save it to the state and remove any displayed error
handleAction' (Data st) (ReceiveInput s) =
    H.put (Data (st { e = Nothing, input = s }))

-- nothing to do if we recieve input on the other states
handleAction' _ (ReceiveInput _) = 
    pure unit

-- move to the edit data view from the results view
handleAction' (Results st) ButtonPress = do
    log (PartyCarloButtonPressed ResultsState)
    log (Transition ResultsState DataState)
    H.put (Data { e : Nothing, input : st.input })

-- pressing the button on the data view will parse the input then run the experiments
handleAction' (Data st) ButtonPress = do
    log (PartyCarloButtonPressed DataState)
    log RunAction
    case parse <<< stripInput $ st.input of
        Left e -> do
            log (ParsingFailed e)
            H.put (Data (st { e = Just e }))
        Right dist -> do
            log (ParsedNProbabilities dist)
            log (Distribution dist)
            log (RunningNExperiments experimentCount)
            H.put Loading
            sleep (Milliseconds 0.0)
            start <- nowDateTime
            exp <- runExperiments experimentCount dist
            case exp of
                Left e -> do
                    log MonteCarloFailed
                    H.put ( Data ( { input: st.input, e: Just e } ) )
                Right r -> do
                    H.put ( Results (
                        { input: st.input
                        , result: r
                        , show: P95
                        } ) )
                    end <- nowDateTime
                    log (CalculationDuration start end)
                    log (Intervals r)

-- nothing to do on other views
handleAction' _ ButtonPress =
    pure unit
