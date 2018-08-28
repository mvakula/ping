module Main where

import Prelude

import AddEndpoint (addEndPoint)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Timer (setInterval)
import Login (login)
import Milkis as M
import React.Basic (ReactComponent, createElement, react)
import React.Basic.DOM as R
import Simple.JSON (read)
import Status (status)
import Types (Endpoint, PingData)
import Utils (baseUrl, fetch)
import Utils as Utils


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
          [ createElement addEndPoint { refreshEndpoints' }
          , createElement login {}
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
  headers <- liftEffect Utils.mkHeaders'
  let opts = { method: M.getMethod, headers }
  res <- attempt $ fetch (M.URL $ baseUrl <> "getEndpoints") opts
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
  headers <- liftEffect Utils.mkHeaders'
  let opts = { method: M.getMethod, headers }
  res <- attempt $ fetch (M.URL $ baseUrl <> "getPings") opts
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
