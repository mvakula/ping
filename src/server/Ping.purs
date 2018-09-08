module Ping where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Newtype (unwrap)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Now (now)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import NotifySlack (notifySlack)
import Types as Types

fetch :: M.Fetch
fetch = M.fetch nodeFetch

nowMillis :: Aff Number
nowMillis = unwrap <<< unInstant <$> liftEffect now

ping :: M.URL -> Aff Types.Ping
ping url = do
  startTime <- nowMillis
  result <- attempt $ fetch url M.defaultFetchOptions
  endTime <- nowMillis
  let latency = round $ endTime - startTime
  case result of
    Right res -> do
      pure { statusCode: M.statusCode res, latency: latency }
    Left e -> do
      logShow e
      notifySlack $ show e
      pure { statusCode: 500, latency: 0 }
