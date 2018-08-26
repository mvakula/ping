module Status where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class.Console (logShow)
import Milkis as M
import React.Basic (JSX, ReactComponent, stateless)
import React.Basic.DOM as R
import React.Basic.Events as Events
import Simple.JSON (writeJSON)
import Types (Endpoint, PingData)
import Utils (baseUrl, fetch)


status :: ReactComponent { endpoint :: Endpoint, refreshEndpoints' :: Aff Unit, pings :: Array PingData }
status = stateless
  { displayName: "Status"
  , render
  }
  where
    render props =
      let
        pingBars = (\ping -> pingBar ping) <$> props.pings
        avg = R.div { children:
          [ R.div { className: "title", children: [ R.text "AVG" ] }
          , R.div { children: [ R.text $ (show $ avgLatency props.pings) <> " ms" ] }
          ]}
        name = R.div { children:
          [ R.div { className: "title", children: [ R.text props.endpoint.name ] }
          , R.div { className: "url", children: [ R.text props.endpoint.url ] }
          ]}
        deleteBtn = R.button
          { children: [ R.text "Delete" ]
          , onClick: Events.handler_ do
                      launchAff_ do
                        res <- deleteEndpoint props.endpoint.id
                        case res of
                          Just success -> do
                            props.refreshEndpoints'
                          Nothing -> do
                            pure unit
          }
      in
        R.div { className: "status", children:
          [ R.div { className: "name-and-url", children: [ name ] }
          , R.div { className: "avg", children: [ avg ] }
          , R.div { className: "delete-btn-container", children: [ deleteBtn ] }
          , R.div { className: "pings", children: pingBars }
          ]
        }

pingBar :: PingData -> JSX
pingBar ping = R.div
  { className: "ping-bar" <> if ping.statusCode /= 200 then " error" else ""
  , style: R.css { height: ping.latency / 20 }
  , title: show ping.latency
  }

avgLatency :: Array (PingData) -> Int
avgLatency pings =
  sum' / length'
  where
    sum' = sum $ (\p -> p.latency) <$> pings
    length' = Array.length pings

deleteEndpoint :: Int -> Aff (Maybe Int)
deleteEndpoint id = do
  let
    body = writeJSON { id }
    opts =
      { method: M.deleteMethod
      , body
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      }
  res <- attempt $ fetch (M.URL $ baseUrl <> "deleteEndpoint") opts
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      if statusCode == 200
        then
          pure $ (Just statusCode)
        else do
          logShow statusCode
          pure Nothing
    Left e -> do
      logShow e
      pure Nothing
