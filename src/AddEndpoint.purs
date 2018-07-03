module AddEndpoint where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Either (Either(..))
import Database.Postgres as PG
import Database.Postgres.SqlValue (toSql)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Simple.JSON (readJSON)

clientConfig :: PG.ClientConfig
clientConfig =
  { host: "localhost"
  , database: "ping"
  , port: 5432
  , user: "mvakula"
  , password: ""
  , ssl: false
  }

connectionInfo :: PG.ConnectionInfo
connectionInfo = PG.connectionInfoFromConfig clientConfig PG.defaultPoolConfig

type Body = {
  endpoint :: String
}

type Res =
  { statusCode :: Int
  , body :: String
  }

main :: String -> Effect (Promise Res)
main body = fromAff do
  case readJSON body of
    Left e -> do
      logShow e
      pure { statusCode: 500, body: "Failure" }
    Right (body' :: Body) -> do
      insertEndpoint body'.endpoint
      pure { statusCode: 200, body: "Success" }

insertEndpoint :: String -> Aff Unit
insertEndpoint endpoint = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = PG.Query "INSERT INTO endpoints (url) VALUES ($1)"
    PG.execute queryStr [toSql endpoint] c
    liftEffect $ PG.end pool
