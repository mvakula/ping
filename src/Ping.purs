module Ping where
  
import Prelude

import Control.Promise (Promise, fromAff)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)

main :: M.URL -> Effect (Promise PingData)
main pingUrl = fromAff do ping pingUrl

type PingData =
  { error :: Nullable Error
  , result ::
    { statusCode :: Int
    , latency :: Number
    }
  }

fetch :: M.Fetch
fetch = M.fetch nodeFetch

nowMillis :: Aff Number
nowMillis = unwrap <<< unInstant <$> liftEffect now

ping :: M.URL -> Aff PingData
ping url = do
  startTime <- nowMillis
  result <- attempt $ fetch url M.defaultFetchOptions
  endTime <- nowMillis
  let latency = endTime - startTime
  case result of
    Right res -> do
      pure
          { error: toNullable Nothing
          , result: { statusCode: M.statusCode res, latency: latency }
          }
    Left e -> do
      pure
          { error: toNullable $ Just e
          , result: { statusCode: 0, latency: 0.0 }
          }
