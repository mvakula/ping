module AddEndpoint where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Milkis as M
import React.Basic (ReactComponent, react)
import React.Basic.DOM as R
import React.Basic.DOM.Events as DE
import React.Basic.Events as Events
import Simple.JSON (writeJSON)
import Utils (baseUrl, fetch)
import Utils as Utils

addEndPoint :: ReactComponent { refreshEndpoints' :: Aff Unit }
addEndPoint = react
  { displayName: "addNewEndPoint"
  , initialState
  , receiveProps
  , render
  }
  where
    initialState =
      { name: ""
      , url: ""
      }
    receiveProps _ _ _ = pure unit
    render props state setState =
      let
        handleOnChangeName =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { name = fromMaybe "" value }
        handleOnChangeUrl =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { url = fromMaybe "" value }
        handleOnSubmit =
          Events.handler
            DE.preventDefault $
              \_ -> launchAff_ do
                let
                  body = writeJSON
                    { name: state.name
                    , url: state.url
                    }
                  opts =
                    { method: M.postMethod
                    , body
                    , headers: Utils.mkHeaders
                    }
                res <- attempt $ fetch (M.URL $ baseUrl <> "addEndpoint") opts
                case res of
                  Right response -> do
                    props.refreshEndpoints'
                    liftEffect $ setState _ { name = "", url = "" }
                  Left e -> do
                    logShow e
      in
        R.div { children:
          [ R.h3 { children: [ R.text "Add new endpoint" ] }
          , R.form
            { className: "add-endpoint"
            , onSubmit: handleOnSubmit
            , children:
              [ R.div
                { className: "form-row", children:
                  [ R.label
                    { htmlFor: "name"
                    , children: [ R.text "name" ]
                    }
                  , R.input
                    { type: "text"
                    , id: "name"
                    , required: true
                    , value: state.name
                    , onChange: handleOnChangeName
                    }
                  ]
                }
              , R.div
                { className: "form-row", children:
                  [ R.label
                    { htmlFor: "url"
                    , children: [ R.text "url" ]
                    }
                  , R.input
                    { type: "text"
                    , id: "url"
                    , required: true
                    , value: state.url
                    , onChange: handleOnChangeUrl
                    }
                  ]
                }
              , R.div
                { className: "form-row", children:
                  [ R.input
                    { type: "submit"
                    , className: "submit"
                    , value: "Submit"
                    }
                  ]
                }
              ]
            }
          ]
        }
