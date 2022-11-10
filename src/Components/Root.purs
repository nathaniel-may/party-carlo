module PartyCarlo.Components.Root where

-- import Prelude

-- import Data.Either (hush)
-- import Data.Foldable (elem)
-- import Data.Maybe (Maybe(..), fromMaybe, isJust)
-- import Effect.Aff.Class (class MonadAff)
-- import Halogen (liftEffect)
-- import Halogen as H
-- import Halogen.HTML as HH
-- import Halogen.Store.Connect (Connected, connect)
-- import Halogen.Store.Monad (class MonadStore)
-- import Halogen.Store.Select (selectEq)
-- import Routing.Duplex as RD
-- import Routing.Hash (getHash)
-- import Type.Proxy (Proxy(..))


-- data Route
--   = Data
--   | Results

-- type State =
--   { route :: Maybe Route
--   , loading :: Boolean
--   , input :: String
--   , showError :: Boolean
--   , parsed :: Either Error (Array Probability)
--   , result :: Maybe Result
--   }

-- type ChildSlots =
--   ( home :: OpaqueSlot Unit
--   , login :: OpaqueSlot Unit
--   , register :: OpaqueSlot Unit
--   , settings :: OpaqueSlot Unit
--   , editor :: OpaqueSlot Unit
--   , viewArticle :: OpaqueSlot Unit
--   , profile :: OpaqueSlot Unit
--   )

-- component :: ∀ query m. MonadAff m => H.Component query Unit Void m
-- component = connect (selectEq _.currentUser) $ H.mkComponent
--   { initialState: \{ context: currentUser } -> { route: Nothing, currentUser }
--   , render
--   , eval: H.mkEval $ H.defaultEval
--       { handleQuery = handleQuery
--       , handleAction = handleAction
--       , receive = Just <<< Receive
--       , initialize = Just Initialize
--       }
--   }
--   where
--   handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
--   handleAction = case _ of
--     Initialize -> do
--       -- first we'll get the route the user landed on
--       initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
--       -- then we'll navigate to the new route (also setting the hash)
--       navigate $ fromMaybe Home initialRoute

--     Receive { context: currentUser } ->
--       H.modify_ _ { currentUser = currentUser }

--   handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
--   handleQuery = case _ of
--     Navigate dest a -> do
--       { route, currentUser } <- H.get
--       -- don't re-render unnecessarily if the route is unchanged
--       when (route /= Just dest) do
--         -- don't change routes if there is a logged-in user trying to access
--         -- a route only meant to be accessible to a not-logged-in session
--         case (isJust currentUser && dest `elem` [ Login, Register ]) of
--           false -> H.modify_ _ { route = Just dest }
--           _ -> pure unit
--       pure (Just a)

--   render :: State -> H.ComponentHTML Action ChildSlots m
--   render Input = HH.slot_ (Proxy :: _ "home") unit Home.component unit
--   render Results = HH.slot_ (Proxy :: _ "home") unit Home.component unit
