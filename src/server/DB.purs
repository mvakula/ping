module DB where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Database.Postgres as PG
import Database.Postgres.SqlValue (toSql)
import Effect (Effect)
import Effect.Aff (Aff, Error, error)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Milkis as M
import Node.Process (lookupEnv)
import Ping as Ping
import Simple.JSON as JSON
import Types (Endpoint)
import Types as Types


getEnv :: String -> String
getEnv env =
  fromMaybe "" (unsafePerformEffect $ lookupEnv env)

defaultConfig :: PG.ClientConfig
defaultConfig =
  { host: getEnv "PG_ENDPOINT"
  , database: getEnv "PG_DB"
  , port: 5432
  , user: getEnv "PG_USER"
  , password: getEnv "PG_PW"
  , ssl: true
  }

connectionInfo :: PG.ConnectionInfo
connectionInfo =
   PG.connectionInfoFromConfig defaultConfig PG.defaultPoolConfig

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
    Right (body' :: Types.EndpointBody ()) -> do
      insertEndpoint' body'
      pure { statusCode: 200, body: "Success" }

insertEndpoint' :: Types.EndpointBody () -> Aff Unit
insertEndpoint' { url, name } = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = PG.Query "INSERT INTO endpoints (url, name) VALUES ($1, $2)"
    PG.execute queryStr [toSql url, toSql name] c
    liftEffect $ PG.end pool

insertPing :: { endpointId :: Int, latency :: Int, statusCode :: Int } -> Aff Unit
insertPing { endpointId, latency, statusCode } = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = PG.Query "INSERT INTO pings (endpoint_id, latency, status_code) VALUES ($1, $2, $3)"
    PG.execute queryStr [toSql endpointId, toSql latency, toSql statusCode] c
    liftEffect $ PG.end pool

pingServices :: Effect (Promise Unit)
pingServices = fromAff do
  endpoints <- getEndpoints'
  _ <- for endpoints \endpoint -> do
    ping <- Ping.ping (M.URL endpoint.url)
    insertPing
      { endpointId: endpoint.id
      , latency: ping.latency
      , statusCode: ping.statusCode
      }
  pure unit

getEndpoints :: Effect (Promise (Array Endpoint) )
getEndpoints = fromAff do
  endpoints <- getEndpoints'
  pure endpoints


getEndpoints' :: Aff (Array Endpoint)
getEndpoints' = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = (PG.Query "SELECT * FROM endpoints" :: PG.Query Endpoint )
    PG.query_ read' queryStr c

type PingColumns =
  { id :: Int
  , endpoint_id :: Int
  , latency :: Int
  , status_code :: Int
  }

getPings' :: Aff (Array Types.PingData)
getPings' = do
  pool <- liftEffect $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    let queryStr = (PG.Query "SELECT * FROM pings WHERE timestamp > now() - interval '1 day'" :: PG.Query PingColumns )
    pings <- PG.query_ read' queryStr c
    pure $ pingsFromColumns <$> pings

pingsFromColumns :: PingColumns -> Types.PingData
pingsFromColumns p =
  { id: p.id
  , endpointId: p.endpoint_id
  , latency: p.latency
  , statusCode: p.status_code
  }

getPings :: Effect (Promise (Array Types.PingData))
getPings = fromAff do
  pings <- getPings'
  pure pings

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
