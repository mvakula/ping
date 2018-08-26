module Utils where

import Milkis.Impl.Window (windowFetch)
import Milkis as M

foreign import baseUrl :: String

fetch :: M.Fetch
fetch = M.fetch windowFetch
