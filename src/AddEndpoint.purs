module AddEndpoint where

import Prelude

import Control.Promise (Promise, fromAff)
import Effect (Effect)

main :: Effect (Promise String)
main = fromAff do pure "hello"
