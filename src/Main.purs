module Main where

import Prelude

import Control.Monad.Error.Class (class MonadError, liftEither)
import Data.Array (cons, length, filter, snoc, uncons, zip)
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Data.String (null, trim)
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (debug, error, log, warn)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
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
  { loading :: Boolean
  , input :: String
  , showError :: Boolean
  , parsed :: Either Error (Array Probability)
  , result :: Maybe Result
  }

type Result = 
  { dist :: SortedArray Int
  , p90 :: Tuple Int Int 
  , p95 :: Tuple Int Int 
  , p99 :: Tuple Int Int 
  , p999 :: Tuple Int Int 
  , showBars :: Maybe Interval
  }

data Interval
  = P90
  | P95
  | P99
  | P999

data Error
    = InvalidNumber String
    | InvalidProbability String Number

displayError :: Error -> String
displayError (InvalidNumber s) = "Invalid Number '" <> show s <> "' "
displayError (InvalidProbability _ n) = "Invalid Probability '" <> show n <> "' "

data Action 
    = RunExperiments
    | EditData
    | Parse String
    | ShowBars Interval

component :: ∀ m a b c. MonadAff m => Component a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ i. i -> State
initialState _ = 
  { loading: false
  , input: ""
  , parsed: Right []
  , result: Nothing
  , showError: false
  }

stripInput :: String -> Array String
stripInput s = filter (not null) $ trim <$> lines s

parse :: Array String -> Either Error (Array Probability)
parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< probability =<< parseNum s) <$> input

parseNum :: String -> Either Error Number
parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

resultsDiv :: ∀ a. State -> HTML a Action
resultsDiv st = case st.result of
    Nothing -> HH.div_ []
    Just result -> let
      heights' = Int.toNumber <<< fst <$> (group result.dist)
      maxHeight = 250.0
      maxWidth = 700.0
      barCount = length heights
      barWidth = (maxWidth / Int.toNumber barCount) + 1.0
      factor = maxHeight / arrayMax 0.0 heights'
      heights = (factor * _) <$> heights'
      xoffsets = iterate (barCount) (_ + maxWidth / Int.toNumber barCount) 0.0
    in
      HH.div [ HP.class_ (H.ClassName "chart-div") ]
        [ SE.svg 
          [ SA.class_ (H.ClassName "chart"), SA.viewBox 0.0 0.0 maxWidth maxHeight, SA.preserveAspectRatio Nothing SA.Meet ]
          (arrayAppend 
            (uncurry (barWithHeight barWidth maxHeight) <$> zip heights xoffsets)
            (riemannBars maxHeight barWidth result.showBars heights))
        , HH.div [ HP.class_ (H.ClassName "hcontainer") ] 
            [ HH.div
            [ HP.class_ (H.ClassName "p")
            , HP.id "p90" 
            , HE.onMouseOver (\_ -> ShowBars P90)]
            [ HH.text $ "p90\n" <> showTuple result.p90 ] 
            , HH.div
            [ HP.class_ (H.ClassName "p")
            , HP.id "p95" 
            , HE.onMouseOver (\_ -> ShowBars P95) ]
            [ HH.text $ "p95\n" <> showTuple result.p95 ] 
            , HH.div
            [ HP.class_ (H.ClassName "p")
            , HP.id "p99" 
            , HE.onMouseOver (\_ -> ShowBars P99) ]
            [ HH.text $ "p99\n" <> showTuple result.p99 ] 
            , HH.div
            [ HP.class_ (H.ClassName "p")
            , HP.id "p999" 
            , HE.onMouseOver (\_ -> ShowBars P999) ]
            [ HH.text $ "p99.9\n" <> showTuple result.p999 ] ] ]

riemannBars :: ∀ a. Number -> Number -> Maybe Interval -> Array Number -> Array (HTML a Action)
riemannBars h barWidth interval dist = let
    total = foldr (+) 0.0 dist
  in
    case interval of
      Nothing -> []
      Just P90 -> [ riemannBar (barWidth * (riemannBound (total * 0.1) dist)) h
                  , riemannBar (barWidth * (riemannBound (total - (total * 0.1))) dist) h ]
      Just P95 -> [ riemannBar (barWidth * (riemannBound (total * 0.05) dist)) h
                  , riemannBar (barWidth * (riemannBound (total - (total * 0.05)) dist)) h ]
      Just P99 -> [ riemannBar (barWidth * (riemannBound (total * 0.01) dist)) h
                  , riemannBar (barWidth * (riemannBound (total - (total * 0.01)) dist)) h ]
      Just P999 -> [ riemannBar (barWidth * (riemannBound (total * 0.001) dist)) h
                   , riemannBar (barWidth * (riemannBound (total - (total * 0.001)) dist)) h ]

riemannBar :: ∀ a. Number -> Number -> HTML a Action
riemannBar x h =  SE.rect [SA.class_ (H.ClassName "marker"), SA.x x, SA.y 0.0, SA.width 4.0, SA.height h]

barWithHeight :: ∀ a. Number -> Number -> Number -> Number -> HTML a Action
barWithHeight barWidth maxHeight barHeight xoffset = 
  SE.rect [SA.class_ (H.ClassName "bar"), SA.x xoffset, SA.y (maxHeight - barHeight), SA.width barWidth, SA.height barHeight]

group :: ∀ a. Eq a => SortedArray a -> Array (Tuple Int a)
group xs = foldr foo [] (SortedArray.toArray xs) where
  foo x ys = case uncons ys of
    Nothing -> [(Tuple 1 x)]
    Just { head: (Tuple i x'), tail: t } -> if x == x' 
                                            then cons (Tuple (i + 1) x') t 
                                            else cons (Tuple 1 x) ys

iterate :: ∀ a. Int -> (a -> a) -> a -> Array a
iterate count _ _ | count <= 0 = []
iterate count f x = cons x' (iterate (count - 1) f x')
  where x' = f x

arrayMax :: ∀ a. Ord a => a -> Array a -> a
arrayMax = foldr max

arrayAppend :: ∀ a. Array a -> Array a -> Array a
arrayAppend xs ys = case uncons ys of
  Nothing -> xs
  Just { head: h, tail: t } -> arrayAppend (snoc xs h) t

riemannBound :: Number -> Array Number -> Number
riemannBound x xs = case uncons xs of
  Nothing -> 0.0
  -- Just _ -> spy "riemanBound on x=" x -- TODO block this
  Just { head: h, tail: t } -> if h < x 
                               then 1.0 + riemannBound (x - h) t 
                               else x / h

inputDiv :: ∀ a. State -> HTML a Action
inputDiv st = case st.result of 
  Just _ -> HH.div_ []
  Nothing -> HH.textarea
    [ HP.disabled st.loading
    , HP.id "input"
    , HP.value st.input
    -- TODO this is more frequent than we need. Other option?
    , HE.onValueInput Parse
    ]

errorDiv :: ∀ a. State -> HTML a Action
errorDiv st = if not st.showError 
              then HH.div_ [] 
              else let 
    msg = case st.parsed of
      Right _ -> ""
      Left (InvalidNumber s) -> "\"" <> s <> "\"" <> " is not a number"
      Left (InvalidProbability s _) -> s <> " is not a probability (between 0 and 1)"
    in 
      HH.text msg
    

render :: ∀ a. State -> HTML a Action
render st =
    HH.div
        [ HP.class_ (H.ClassName "vcontainer") ]
        [ HH.h1_ [ HH.text "Party Carlo" ]
        , HH.button
            [ HP.disabled st.loading
            , HP.id "RunExperiments"
            , HP.type_ HP.ButtonButton
            , HE.onClick (\_ -> case st.result of
                Nothing -> RunExperiments
                Just _ -> EditData)
            ]
            [ HH.text if st.loading 
                      then "..." 
                      else case st.result of 
                        Just _ -> "Edit Data" 
                        Nothing -> "Run" ]
        , HH.p [HP.class_ (H.ClassName "pcenter")]
          [ HH.text "How many people do you expect to attend your party?" ]
        , HH.p_
          [ HH.text "Put in a probability for how likely it is for each person to attend and this will use Monte Carlo experiments to give you confidence intervals for what you think the group's attendance will be." ]
        , errorDiv st
        , resultsDiv st
        , inputDiv st
        , HH.footer_
            [ HH.text "PureScript + Netlify | Source on " 
            , HH.a 
                [ HP.href "https://github.com/nathaniel-may/party-carlo" ] 
                [ HH.text "GitHub" ]
            ]
        ]

handleAction :: ∀ o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit

handleAction EditData = do
  H.modify_ (_ { result = Nothing })
  debug $ "returning to edit view"

handleAction (ShowBars interval) = do
  debug $ "ShowBars action called"
  st <- H.get
  -- TODO should just split out results into it's own component with its own state?
  let result = (_ { showBars = Just interval } ) <$> st.result 
  H.modify_ (_ { result = result })

handleAction (Parse s) = do
    H.modify_ (_ { input = s, showError = false })
    case parse <<< stripInput $ s of
        Left e -> do
          debug $ "parsing failed: " <> displayError e
          H.modify_ (_ { parsed = Left e })
        Right parsed -> do 
          H.modify_ (_ { parsed = Right parsed })
          debug $ "parsed " <> (show $ length parsed) <> " probabilities"

handleAction RunExperiments = do
    debug "run action initiated"
    H.modify_ (_ { showError = true })
    let count = 100000
    st <- H.get
    case st.parsed of
        Left _ -> warn "no parsed values to run on"
        Right dist -> do
            log $ "running " <> show count <> " experiments ..."
            H.modify_ (_ { loading = true })
            start <- H.liftEffect $ map toDateTime now
            samples <- H.liftEffect $ sample dist count
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
                                                 , showBars: Nothing },
                                   loading = false })
                    end <- H.liftEffect $ map toDateTime now
                    log $ "result calculated in " <> show (diff end start :: Milliseconds) <> ":"
                    debug $ "result set: " <> (showTuple4 t4)
