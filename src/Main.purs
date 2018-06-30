module Main where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Timer (setInterval)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (ReactComponent, react)
import React.Basic.DOM as R
import Simple.JSON (read)
import Unsafe.Coerce (unsafeCoerce)


baseUrl :: String
baseUrl = "http://localhost:3000/ping/"

url :: String
url = baseUrl <> "?url=https://google.com"

fetch :: M.Fetch
fetch = M.fetch windowFetch

type Status =
  { statusCode :: Int
  , latency :: Number
  }

type State = {
  status :: Array Status
}

main :: ReactComponent {}
main = react { displayName: "Main", initialState, receiveProps, render }
  where
    initialState =
      { status: [
        { statusCode: 0
        , latency: 0.0
        } ]
      }
    receiveProps props state setState = launchAff_ do
      let setState' = liftEffect <<< setState
      _ <- liftEffect $ setInterval 1000 $ launchAff_ do
        status <- getPingStatus
        setState' \s -> s { status = s.status <> [status]}
      pure unit

    render props state setState =
      let
        latencyDiv status = R.div { children: [ R.text $ show status.latency ]}
        latencies = (\status -> latencyDiv status) <$> state.status
      in
        R.div { children: latencies }


getPingStatus :: Aff Status
getPingStatus = do
  res <- attempt $ fetch (M.URL url) M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      case read result of
        Right (status :: Status) -> do
          log $ unsafeCoerce status
          pure status
        Left e -> do
          logShow e
          pure { statusCode: 0, latency: 0.0}
    Left e -> do
      logShow e
      pure { statusCode: 0, latency: 0.0}
