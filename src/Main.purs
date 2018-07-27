module Main where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Timer (setInterval)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX, ReactComponent, createElement, react, stateless)
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
  pings :: Array (Maybe Ping)
}

main :: ReactComponent {}
main = react { displayName: "Main", initialState, receiveProps, render }
  where
    initialState :: State
    initialState =
      { pings: [ Nothing ]
      }
    receiveProps props state setState = launchAff_ do
      let setState' = liftEffect <<< setState
      _ <- liftEffect $ setInterval 1500 $ launchAff_ do
        ping <- getPingStatus
        setState' \s -> s { pings = ping : Array.take 30 s.pings }
      pure unit

    render props state setState =
      let
        pingBars = (\ping -> pingBar ping) <$> state.pings
        avg = R.text $ "Average: " <> (show $ round $ avgLatency state.pings) <> " ms"
      in
        R.div { children:
          [ createElement addNewEndPoint {}
          , R.div { children: [ avg ] }
          , R.div { className: "pings", children: pingBars }
          ]
        }

pingBar :: Maybe Ping -> JSX
pingBar (Just ping) = R.div
  { className: "ping-bar" <> if ping.statusCode /= 200 then " error" else ""
  , style: R.css { height: ping.latency / 10.0 }
  , title: show ping.latency
  }
pingBar (Nothing) = R.div
  { className: "ping-bar error"
  , title: "error"
  }

getPingStatus :: Aff (Maybe Ping)
getPingStatus = do
  res <- attempt $ fetch (M.URL url) M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      case read result of
        Right (ping :: Ping) -> do
          log $ unsafeCoerce ping
          pure (Just ping)
        Left e -> do
          logShow e
          pure Nothing
    Left e -> do
      logShow e
      pure Nothing

avgLatency :: Array (Maybe Ping) -> Number
avgLatency pings =
  sum' / length'
  where
    sum' = sum $ (getLatency) <$> pings
    length' = toNumber $ Array.length pings
    getLatency ping =
      case ping of
        Just p -> p.latency
        Nothing -> 0.0


addNewEndPoint :: ReactComponent {}
addNewEndPoint = stateless { displayName: "addNewEndPoint", render }
  where
    render _ =
      R.input {}
