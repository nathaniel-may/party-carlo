module PartyCarlo.Pages.Data where

import Prelude

import Data.Array (filter, length)
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Number as Number
import Data.String as String
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (debug, error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PartyCarlo.Components.HTML.Button (button)
import PartyCarlo.Components.HTML.Footer (footer)
import PartyCarlo.Components.HTML.Header (header)
import PartyCarlo.Components.Utils (OpaqueSlot)
import PartyCarlo.Utils (mapLeft)
import Probability (Probability, probability)


data Action 
    = Parse String
    | RunExperiments

type State =
  { input :: String
  , showError :: Boolean
  , parsed :: Either Error (Array Probability)
  }

type ChildSlots = (rawHtml :: OpaqueSlot Unit)

data Error
    = InvalidNumber String
    | InvalidProbability String Number

-- | string used to display the error value to the user (suitable for both UI and console logs)
displayError :: Error -> String
displayError (InvalidNumber s) = "\"" <> s <> "\"" <> " is not a number"
displayError (InvalidProbability s _) = s <> " is not a probability (between 0 and 1)"

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
  initialState _ =
    { input : ""
    , showError : false
    , parsed : Right []
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction (Parse s) = do
    H.modify_ (_ { input = s, showError = false })
    case parse <<< stripInput $ s of
        Left e -> do
          debug $ "parsing failed: " <> displayError e
          H.modify_ (_ { parsed = Left e })
        Right parsed -> do 
          H.modify_ (_ { parsed = Right parsed })
          debug $ "parsed " <> (show $ length parsed) <> " probabilities"

  handleAction RunExperiments = error "run experiments not implemented"
      
  parse :: Array String -> Either Error (Array Probability)
  parse input = sequence $ (\s -> mapLeft (InvalidProbability s) <<< probability =<< parseNum s) <$> input

  parseNum :: String -> Either Error Number
  parseNum s = maybe (Left $ InvalidNumber s) Right (Number.fromString s)

  stripInput :: String -> Array String
  stripInput s = filter (not String.null) $ String.trim <$> lines s

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
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
        -- parses the whole dataset on each character typed. Since data for real workloads are
        -- most often copy-pasted, this inefficiency only affects manually entered toy examples.
        , HE.onValueInput Parse
        ]
      , footer
      ]
