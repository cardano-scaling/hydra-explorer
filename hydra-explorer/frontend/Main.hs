{-# language CPP                 #-}
{-# language DeriveAnyClass      #-}
{-# language DeriveGeneric       #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase          #-}
{-# language NamedFieldPuns      #-}
{-# language OverloadedLabels    #-}
{-# language OverloadedStrings   #-}
{-# language PackageImports      #-}

module Main where

import "aeson" Data.Aeson hiding ((.=))
import "base" GHC.Generics (Generic)
import "generic-lens" Data.Generics.Labels ()
import "lens" Control.Lens
import "miso" Miso
import "miso" Miso.Html qualified as H
import "miso" Miso.Html.Event qualified as E
import "miso" Miso.Html.Property qualified as P
import "text" Data.Text (Text)


----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { observations :: [Observation]
  } deriving (Show, Eq, Generic)


----------------------------------------------------------------------------
data Observation = Observation
  { blockNo :: Int
  , contestations :: Int
  , status :: Text
  , network :: Text
  , version :: Text
  }
  deriving (Show, Eq, Generic, FromJSON)


----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = FetchData
  | UpdateData (Response [Observation])
  | ErrorHandler (Response MisoString)


----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp app)


----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel


----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model []


----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  FetchData ->
    getJSON "https://explorer.hydra.family/heads" [] UpdateData ErrorHandler

  UpdateData (Response {body}) -> do
    #observations .= body

  ErrorHandler (Response {body}) -> do
    io_ (consoleError body)


----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel x =
  H.div_ []
  [ H.div_ [ P.className "counter" ] (map viewObservation (observations x))
  , H.button_ [ E.onClick FetchData ] [ text "Fetch" ]
  ]

viewObservation :: Observation -> View Model Action
viewObservation o =
  H.div_ [] [ text $ ms $ network o ]
