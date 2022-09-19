module Main where

import Prelude

import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Array (length)
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import MonteCarlo (confidenceInterval, sample)
import Probability (p90, p95, p99, p999, probability, Probability)
import SortedArray (SortedArray)
import SortedArray as SortedArray


data Tuple4 a b c d = Tuple4 a b c d

showTuple :: ∀ a b. Show a => Show b => Tuple a b -> String
showTuple (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"

showTuple4 :: ∀ a b c d. Show a => Show b => Show c => Show d => Tuple4 a b c d -> String
showTuple4 (Tuple4 a b c d) = "(" <> show a <> ", " <> show b <> ", " <> show c <> ", " <> show d <> ")"

probErr :: Number -> String
probErr n = "error parsing distribution probabilities: " <> show n <> " is not between 0.0 and 1.0"

mapLeft :: ∀ a e' e. (e -> e') -> Either e a -> Either e' a
mapLeft f = either (Left <<< f) Right

note :: ∀ e a. e -> Maybe a -> Either e a
note e = maybe (Left e) Right

noteT :: ∀ e m a. MonadError e m => e -> Maybe a -> m a
noteT e = liftEither <<< note e


---- Halogen ---- 

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
  { dist :: Maybe (SortedArray Int)
  , loading :: Boolean
  , input :: String
  , parsed :: Maybe (Array Probability)
  , result :: Maybe ({ p90 :: Tuple Int Int 
                     , p95 :: Tuple Int Int 
                     , p99 :: Tuple Int Int 
                     , p999 :: Tuple Int Int 
                     })
  , error :: Maybe (Error)
  }

data Error
    = InvalidNumber String
    | InvalidProbability Number
    | InternalError

displayError :: Error -> String
displayError (InvalidNumber s) = "Invalid Number '" <> show s <> "' "
displayError (InvalidProbability n) = "Invalid Probability '" <> show n <> "' "
displayError (InternalError) = "Internal Error"

data Action 
    = RunExperiments
    | Parse String

component :: ∀ m a b c. MonadAff m => Component a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ i. i -> State
initialState _ = 
  { dist: Nothing
  , loading: false
  , input: ""
  , parsed: Nothing
  , result: Nothing
  , error: Nothing
  }

parse :: String -> Either Error (Array Probability)
parse s = do
  nums <- sequence $ parseNum <$> lines s
  sequence $ (mapLeft InvalidProbability <<< probability) <$> nums

parseNum :: String -> Either Error Number
parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

resultsDiv :: ∀ a. State -> HTML a Action
resultsDiv st = case st.result of
    Nothing -> HH.div [ HP.class_ (H.ClassName "results") ] []
    Just result -> HH.div 
        [ HP.class_ (H.ClassName "results") ] 
        [ HH.h2_ [ HH.text ("90% - " <> showTuple result.p90) ] 
        , HH.h2_ [ HH.text ("95% - " <> showTuple result.p95) ] 
        , HH.h2_ [ HH.text ("99% - " <> showTuple result.p99) ] 
        , HH.h2_ [ HH.text ("99.9% - " <> showTuple result.p999) ] 
        ]

render :: ∀ a. State -> HTML a Action
render st =
    HH.div
        [ HP.class_ (H.ClassName "VContainer") ]
        [ HH.div_
            [ HH.div
                [ HP.class_ (H.ClassName "HContainer") ]
                [ HH.h1_ [ HH.text "Party Carlo" ] ]
            , HH.div
                [ HP.class_ (H.ClassName "VContainer") ] 
                [ HH.button
                    [ HP.disabled st.loading
                    , HP.id "RunExperiments"
                    , HP.type_ HP.ButtonButton
                    , HE.onClick (\_ -> RunExperiments)
                    ]
                    [ HH.text if st.loading then "Experimenting..." else "Run Experiments" ]
                ]
            , resultsDiv st
            , HH.div
                [ HP.class_ (H.ClassName "input") ]  
                [ HH.textarea
                    [ HP.disabled st.loading
                    , HP.id "input"
                    , HP.value st.input
                    -- TODO this is more frequent than we need. Other option?
                    , HE.onValueInput Parse
                    ]
                ]
            , HH.div
                [ HP.class_ (H.ClassName "VContainer") ]
                [ HH.footer_
                    [ HH.text "PureScript + Netlify | Source on " 
                    , HH.a 
                        [ HP.href "https://github.com/nathaniel-may/party-carlo" ] 
                        [ HH.text "GitHub" ]
                    ]
                ]
            ]
        ]

handleAction :: ∀ o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit

handleAction (Parse s) = do
    case parse s of
        Left e -> do
          log $ "parsing failed: " <> displayError e
          H.modify_ (_ { error = Just e })
        Right parsed -> do 
          H.modify_ (_ { parsed = Just parsed })
          log $ "parsed " <> (show $ length parsed) <> " probabilities"

handleAction RunExperiments = do
    log "run action initiated"
    st <- H.get
    let count = 100000
    case st.parsed of
        Nothing -> log "no parsed values to run on"
        Just dist -> do
            log $ "running " <> show count <> " experiments ..."
            H.modify_ (_ { loading = true })
            start <- H.liftEffect $ map toDateTime now
            samples <- H.liftEffect $ sample dist count
            let sorted = SortedArray.fromArray samples
            H.modify_ (_ { dist = Just sorted })
            let m4 = ( 
                Tuple4 <$> confidenceInterval p90 sorted
                <*> confidenceInterval p95 sorted
                <*> confidenceInterval p99 sorted
                <*> confidenceInterval p999 sorted)
            case m4 of
                Nothing -> do
                    H.modify_ (_ { error = Just InternalError })
                    log "confidence interval calculation failed"
                Just t4@(Tuple4 p90val p95val p99val p999val) -> do
                    H.modify_ (_ { result = Just { p90: p90val
                                                 , p95: p95val
                                                 , p99: p99val
                                                 , p999: p999val } })
                    H.modify_ (_ { loading = false, error = Nothing })
                    end <- H.liftEffect $ map toDateTime now
                    log $ "result calculated in " <> show (diff end start :: Milliseconds) <> ":"
                    log $ showTuple4 t4
