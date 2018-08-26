module Types where

type EndpointBody r =
  { name :: String
  , url :: String
  | r
  }

type Endpoint = EndpointBody ( id :: Int )

type Ping =
  { statusCode :: Int
  , latency :: Int
  }

type PingData =
  { id :: Int
  , endpointId :: Int
  , latency :: Int
  , statusCode :: Int
  }
