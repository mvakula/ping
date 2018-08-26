module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Timer (setInterval)
import Milkis as M
import React.Basic (ReactComponent, createElement, react)
import React.Basic.DOM as R
import React.Basic.DOM.Events as DE
import React.Basic.Events as Events
import Simple.JSON (read, writeJSON)
import Status (status)
import Types (Endpoint, PingData)
import Utils (baseUrl, fetch)


type State =
  { endpoints :: Array Endpoint
  , pings :: Array PingData
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
      intervalId <- liftEffect $ setInterval 30000 $ launchAff_ do
        refreshPings setState'
      refreshPings setState'
    render props state setState =
      let
        setState' = liftEffect <<< setState
        refreshEndpoints' = refreshEndpoints setState'
        filterPings id = Array.filter (\p -> p.endpointId == id) state.pings
      in
        R.div { children:
          [ createElement addNewEndPoint { refreshEndpoints' }
          ] <> ((\endpoint -> createElement status { endpoint, refreshEndpoints', pings: (filterPings endpoint.id) }) <$> state.endpoints)
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

getEndpoints :: Aff (Maybe (Array Endpoint))
getEndpoints = do
  res <- attempt $ fetch (M.URL $ baseUrl <> "getEndpoints") M.defaultFetchOptions
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

getPings :: Aff (Maybe (Array PingData))
getPings = do
  res <- attempt $ fetch (M.URL $ baseUrl <> "getPings") M.defaultFetchOptions
  case res of
    Right response -> do
      let statusCode = M.statusCode response
      result <- M.json response
      case read result of
        Right (pings :: Array (PingData)) -> do
          pure (Just pings)
        Left e -> do
          logShow e
          pure Nothing
    Left e -> do
      logShow e
      pure Nothing

addNewEndPoint :: ReactComponent { refreshEndpoints' :: Aff Unit }
addNewEndPoint = react
  { displayName: "addNewEndPoint"
  , initialState
  , receiveProps
  , render
  }
  where
    initialState =
      { name: ""
      , url: ""
      }
    receiveProps _ _ _ = pure unit
    render props state setState =
      let
        handleOnChangeName =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { name = fromMaybe "" value }
        handleOnChangeUrl =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { url = fromMaybe "" value }
        handleOnSubmit =
          Events.handler
            DE.preventDefault $
              \_ -> launchAff_ do
                let
                  body = writeJSON
                    { name: state.name
                    , url: state.url
                    }
                  opts =
                    { method: M.postMethod
                    , body
                    , headers: M.makeHeaders { "Content-Type": "application/json" }
                    }
                res <- attempt $ fetch (M.URL $ baseUrl <> "addEndpoint") opts
                case res of
                  Right response -> do
                    props.refreshEndpoints'
                  Left e -> do
                    logShow e
      in
        R.form
          { className: "add-endpoint"
          , onSubmit: handleOnSubmit
          , children:
            [ R.div
              { className: "form-row", children:
                [ R.label
                  { htmlFor: "name"
                  , children: [ R.text "name" ]
                  }
                , R.input
                  { type: "text"
                  , id: "name"
                  , required: true
                  , onChange: handleOnChangeName
                  }
                ]
              }
            , R.div
              { className: "form-row", children:
                [ R.label
                  { htmlFor: "url"
                  , children: [ R.text "url" ]
                  }
                , R.input
                  { type: "text"
                  , id: "url"
                  , required: true
                  , onChange: handleOnChangeUrl
                  }
                ]
              }
            , R.div
              { className: "form-row", children:
                [ R.input
                  { type: "submit"
                  , className: "submit"
                  , value: "Submit"
                  }
                ]
              }
            ]
          }
