module DB where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Database.Postgres as PG
import Database.Postgres.SqlValue (toSql)
import Effect (Effect)
import Effect.Aff (Aff, Error, error)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Foreign (Foreign)
import Simple.JSON as JSON
import Types (Endpoint)

clientConfig :: PG.ClientConfig
clientConfig =
  { host: "localhost"
  , database: "postgres"
  , port: 32772
  , user: "postgres"
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

read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read

insertEndpoint :: String -> Effect (Promise Res)
insertEndpoint body = fromAff do
  case JSON.readJSON body of
    Left e -> do
      logShow e
      pure { statusCode: 500, body: "Failure" }
    Right (body' :: Body) -> do
      insertEndpoint' body'.endpoint
      pure { statusCode: 200, body: "Success" }

insertEndpoint' :: String -> Aff Unit
insertEndpoint' endpoint = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = PG.Query "INSERT INTO endpoints (url) VALUES ($1)"
    PG.execute queryStr [toSql endpoint] c
    liftEffect $ PG.end pool


getEndpoints :: Effect (Promise { statusCode :: Int, body :: (Array Endpoint) })
getEndpoints = fromAff do
  endpoints <- getEndpoints'
  pure { statusCode: 200, body: endpoints }


getEndpoints' :: Aff (Array Endpoint)
getEndpoints' = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = (PG.Query "SELECT * FROM endpoints" :: PG.Query Endpoint )
    PG.query_ read' queryStr c

deleteEndpoint :: String -> Effect (Promise Res)
deleteEndpoint body = fromAff do
  case JSON.readJSON body of
    Left e -> do
      logShow e
      pure { statusCode: 500, body: "Failure" }
    Right (body' :: { id :: Int }) -> do
      deleteEndpoint' body'.id
      pure { statusCode: 200, body: "Success" }

deleteEndpoint' :: Int -> Aff Unit
deleteEndpoint' id = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = PG.Query "DELETE FROM endpoints WHERE id in ($1)"
    PG.execute queryStr [toSql id] c
