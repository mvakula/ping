module Main where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (round, toNumber)
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

type Ping =
  { statusCode :: Int
  , latency :: Number
  }

type State = {
  pings :: Array Ping
}

main :: ReactComponent {}
main = react { displayName: "Main", initialState, receiveProps, render }
  where
    initialState =
      { pings: [
        { statusCode: 0
        , latency: 0.0
        } ]
      }
    receiveProps props state setState = launchAff_ do
      let setState' = liftEffect <<< setState
      _ <- liftEffect $ setInterval 1500 $ launchAff_ do
        ping <- getPingStatus
        setState' \s -> s { pings = ping : Array.take 30 s.pings }
      pure unit

    render props state setState =
      let
        latencyDiv ping = R.div
          { className: "ping-bar"
          , style: R.css { height: ping.latency / 10.0 }
          , title: show ping.latency
          }
        latencies = (\ping -> latencyDiv ping) <$> state.pings
        avg = R.text $ "Average: " <> (show $ round $ avgLatency state.pings) <> " ms"
      in
        R.div { children:
          [ R.div { children: [ avg ] }
          , R.div { className: "pings", children: latencies }
          ]
        }


getPingStatus :: Aff Ping
getPingStatus = do
  res <- attempt $ fetch (M.URL url) M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      case read result of
        Right (ping :: Ping) -> do
          log $ unsafeCoerce ping
          pure ping
        Left e -> do
          logShow e
          pure { statusCode: 0, latency: 0.0}
    Left e -> do
      logShow e
      pure { statusCode: 0, latency: 0.0}

avgLatency :: Array Ping -> Number
avgLatency pings =
  sum' / length'
  where
    sum' = sum $ (\ping -> ping.latency) <$> pings
    length' = toNumber $ Array.length pings
