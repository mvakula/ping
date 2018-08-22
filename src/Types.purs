module Types where


type Endpoint =
  { id :: Int
  , url :: String
  }

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
