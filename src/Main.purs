module Main where

import Prelude

import Types (Endpoint)
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Timer (setInterval)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX, ReactComponent, createElement, react)
import React.Basic.DOM as R
import React.Basic.DOM.Events as DE
import React.Basic.Events as Events
import Simple.JSON (read, writeJSON)
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

getEndpoints :: Aff (Maybe (Array Endpoint))
getEndpoints = do
  res <- attempt $ fetch (M.URL "http://localhost:3000/getEndpoints") M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      log $ unsafeCoerce result
      case read result of
        Right (endpoints :: Array (Endpoint)) -> do
          log $ unsafeCoerce endpoints
          pure (Just endpoints)
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
addNewEndPoint = react
  { displayName: "addNewEndPoint"
  , initialState
  , receiveProps
  , render
  }
  where
    initialState = { endpoint: "" }
    receiveProps _ _ _ = pure unit
    render props state setState =
      let
        handleOnChange =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { endpoint = fromMaybe "" value }
        handleOnSubmit =
          Events.handler
            DE.preventDefault $
              \_ -> launchAff_ do
                let
                  body = writeJSON { endpoint: state.endpoint }
                  opts =
                    { method: M.postMethod
                    , body
                    , headers: M.makeHeaders { "Content-Type": "application/json" }
                    }
                _ <- attempt $ fetch (M.URL "http://localhost:3000/addEndpoint") opts
                pure unit

      in
        R.form
          { children:
            [ R.input
              { type: "text"
              , onChange: handleOnChange
              }
            , R.input
              { type: "submit"
              , value: "Submit"
              }
            ]
            , onSubmit: handleOnSubmit
          }
