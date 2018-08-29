module Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Record as Record
import Web.HTML (Window, window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as LS

foreign import baseUrl :: String
foreign import btoa :: String -> String

window' :: Window
window' = unsafePerformEffect window

fetch :: M.Fetch
fetch = M.fetch windowFetch

type Creds = { user :: String , pass :: String }

getCreds :: Effect Creds
getCreds = do
  localStorage' <- localStorage window'
  user <- LS.getItem "user" localStorage'
  pass <- LS.getItem "pass" localStorage'
  case user, pass of
    Just user', Just pass' ->
      pure { user: user', pass: pass' }
    _, _ -> do
      log "Username or password not found"
      pure $ { user: "", pass: "" }

mkAuthToken :: Creds -> String
mkAuthToken { user, pass } =
    "Basic " <> (btoa $ user <> ":" <> pass)

getAuthorizationHeader :: Creds -> { "Authorization" :: String }
getAuthorizationHeader creds =
  let token = mkAuthToken creds
  in { "Authorization": token }

mkHeaders :: Object String
mkHeaders =
    M.makeHeaders $ Record.merge (getAuthorizationHeader creds) { "Content-Type": "application/json" }
    where
      creds = unsafePerformEffect getCreds

mkHeaders' :: forall m. MonadEffect m => m (Object String)
mkHeaders' = do
  let creds = unsafePerformEffect getCreds
      header = getAuthorizationHeader creds
  pure $ M.makeHeaders header
