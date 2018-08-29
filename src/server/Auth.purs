module Auth where

import Prelude

import Data.Maybe (fromMaybe)
import Data.String as String
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Process (lookupEnv)

user :: String
user = fromMaybe "user" $ unsafePerformEffect $ lookupEnv "AUTH_USER"

pass :: String
pass = fromMaybe "pass" $ unsafePerformEffect $ lookupEnv "AUTH_PASSWORD"

authString :: String
authString =
  let
    buffer = unsafePerformEffect $ Buffer.fromString (user <> ":" <> pass) UTF8
    base64str = unsafePerformEffect $ Buffer.toString Base64 buffer
  in
    "Basic " <> base64str

type Policy =
  { principalId :: String
  , policyDocument :: PolicyDocument
  }
type PolicyDocument =
  { "Version" :: String
  , "Statement" :: Array Statement
  }
type Statement =
  { "Action" :: String
  , "Effect" :: String
  , "Resource" :: String
  }

generatePolicy :: String -> String -> String -> Policy
generatePolicy principalId effect resource =
  if not String.null effect && not String.null resource
    then
      let statement =
            { "Action": "execute-api:Invoke"
            , "Effect": effect
            , "Resource": resource
            }
          policyDocument =
            { "Version": "2012-10-17"
            , "Statement": [ statement ]
            }
      in
        { principalId, policyDocument }
    else
      let
          policyDocument =
            { "Version": ""
            , "Statement": []
            }
      in
        { principalId, policyDocument }
