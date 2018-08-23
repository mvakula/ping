module Main where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import React.Basic (JSX, ReactComponent, createElement, react)
import React.Basic.DOM as R
import React.Basic.DOM.Events as DE
import React.Basic.Events as Events
import Simple.JSON (read, writeJSON)
import Types (Endpoint, Ping)
import Types as Types


baseUrl :: String
baseUrl = "http://localhost:3000/ping/?url="

fetch :: M.Fetch
fetch = M.fetch windowFetch

type State =
  { endpoints :: Array Endpoint
  , pings :: Array Types.PingData
  }

main :: ReactComponent {}
main = react { displayName: "Main", initialState, receiveProps, render }
  where
    initialState :: State
    initialState =
      { endpoints: []
      , pings: []
      }
    receiveProps props state setState = launchAff_ do
      let setState' = liftEffect <<< setState
      refreshEndpoints setState'
      refreshPings setState'
    render props state setState =
      let
        setState' = liftEffect <<< setState
        refreshEndpoints' = refreshEndpoints setState'
      in
        R.div { children:
          [ createElement addNewEndPoint { refreshEndpoints' }
          ] <> ((\endpoint -> createElement status { endpoint, refreshEndpoints' }) <$> state.endpoints)
        }

refreshEndpoints :: ((State -> State) -> Aff Unit) -> Aff Unit
refreshEndpoints setState' = do
      endpoints <- getEndpoints
      case endpoints of
        Just endpoints' ->
          setState' \s -> s { endpoints = endpoints'}
        Nothing ->
          pure unit

refreshPings :: ((State -> State) -> Aff Unit) -> Aff Unit
refreshPings setState' = do
  pings <- getPings
  case pings of
    Just pings'  ->
      setState' \s -> s { pings = pings' }
    Nothing ->
      pure unit

status :: ReactComponent { endpoint :: Endpoint, refreshEndpoints' :: Aff Unit }
status = react
  { displayName: "Status"
  , initialState
  , receiveProps
  , render
  }
  where
    initialState =
      { pings: [ Nothing ]
      , timerId : Nothing
      }
    receiveProps props state setState = launchAff_ do
      let setState' = liftEffect <<< setState
      intervalId <- liftEffect $ setInterval 1500 $ launchAff_ do
        ping <- getPingStatus props.endpoint.url
        setState' \s -> s { pings = ping : Array.take 30 s.pings }
      setState' \s -> s { timerId = (Just (intervalId)) }
      pure unit
    render props state setState =
      let
        pingBars = (\ping -> pingBar ping) <$> state.pings
        avg = R.div { children:
          [ R.div { children: [ R.text "AVG" ] }
          , R.div { children: [ R.text $ (show $ avgLatency state.pings) <> " ms" ] }
          ]}
        name = R.div { children:
          [ R.div { children: [ R.text "URL" ] }
          , R.div { children: [ R.text props.endpoint.url ] }
          ]}
        deleteBtn = R.button
          { children: [ R.text "Delete" ]
          , onClick: Events.handler_ do
                      launchAff_ do
                        res <- deleteEndpoint props.endpoint.id
                        case res of
                          Just success -> do
                            clearTimer state.timerId
                            props.refreshEndpoints'
                          Nothing -> do
                            pure unit
          }
        clearTimer :: Maybe IntervalId -> Aff Unit
        clearTimer timerId =
          case timerId of
            (Just timerId') -> do
              liftEffect $ clearInterval timerId'
            Nothing -> do
              pure unit

      in
        R.div { className: "status", children:
          [ R.div { className: "name", children: [ name ] }
          , R.div { className: "avg", children: [ avg ] }
          , R.div { className: "avg", children: [ deleteBtn ] }
          , R.div { className: "pings", children: pingBars }
          ]
        }


pingBar :: Maybe Ping -> JSX
pingBar (Just ping) = R.div
  { className: "ping-bar" <> if ping.statusCode /= 200 then " error" else ""
  , style: R.css { height: ping.latency / 20 }
  , title: show ping.latency
  }
pingBar (Nothing) = R.div
  { className: "ping-bar error"
  , title: "error"
  }

getPingStatus :: String -> Aff (Maybe Ping)
getPingStatus url = do
  res <- attempt $ fetch (M.URL (baseUrl <> url)) M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      case read result of
        Right (ping :: Ping) -> do
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
      case read result of
        Right (endpoints :: Array (Endpoint)) -> do
          pure (Just endpoints)
        Left e -> do
          logShow e
          pure Nothing
    Left e -> do
      logShow e
      pure Nothing

getPings :: Aff (Maybe (Array Types.PingData))
getPings = do
  res <- attempt $ fetch (M.URL "http://localhost:3000/getPings") M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      case read result of
        Right (pings :: Array (Types.PingData)) -> do
          pure (Just pings)
        Left e -> do
          logShow e
          pure Nothing
    Left e -> do
      logShow e
      pure Nothing

deleteEndpoint :: Int -> Aff (Maybe Int)
deleteEndpoint id = do
  let
    body = writeJSON { id }
    opts =
      { method: M.deleteMethod
      , body
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      }
  res <- attempt $ fetch (M.URL "http://localhost:3000/deleteEndpoint") opts
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

avgLatency :: Array (Maybe Ping) -> Int
avgLatency pings =
  sum' / length'
  where
    sum' = sum $ (getLatency) <$> pings
    length' = Array.length pings
    getLatency ping =
      case ping of
        Just p -> p.latency
        Nothing -> 0


addNewEndPoint :: ReactComponent { refreshEndpoints' :: Aff Unit }
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
                res <- attempt $ fetch (M.URL "http://localhost:3000/addEndpoint") opts
                case res of
                  Right response -> do
                    props.refreshEndpoints'
                  Left e -> do
                    logShow e

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
