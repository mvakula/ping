module NotifySlack where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.Process (lookupEnv)
import Simple.JSON (writeJSON)

fetch :: M.Fetch
fetch = M.fetch nodeFetch

webhook :: Maybe String
webhook = unsafePerformEffect $ lookupEnv "SLACK_WEBHOOK"

slackChannel :: Maybe String
slackChannel = unsafePerformEffect $ lookupEnv "SLACK_CHANNEL"

notifySlack :: String -> Aff Unit
notifySlack msg = do
  case webhook, slackChannel of
    Just webhook', Just slackChannel' -> do
      _ <- fetch (M.URL webhook')
        { method: M.postMethod
        , headers: M.makeHeaders { "ContentType": "application/json" }
        , body: writeJSON
          { channel: slackChannel'
          , username: "pingbot"
          , text: msg
          , icon_emoji: ":ghost:"
          }
        }
      pure unit
    _, _ -> do
      log "No slack integration configured"
      pure unit

