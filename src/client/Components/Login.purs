
module Login where

import Prelude

import Data.Maybe (fromMaybe)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events as DE
import React.Basic.Events as Events
import Web.HTML (Window, window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as LS

window' :: Window
window' = unsafePerformEffect window

login :: React.Component {}
login = React.component
  { displayName: "Login"
  , initialState
  , receiveProps
  , render
  }
  where
    initialState =
      { user: ""
      , pass: ""
      }
    receiveProps _ = pure unit
    render { props, state, setState } =
      let
        handleOnChangeUser =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { user = fromMaybe "" value }
        handleOnChangePass =
          Events.handler
            DE.targetValue
              \value ->
                setState _ { pass = fromMaybe "" value }
        handleOnSubmit =
          Events.handler
            DE.preventDefault $
              \_ -> do
                localStorage' <- localStorage window'
                LS.setItem "user" state.user localStorage'
                LS.setItem "pass" state.pass localStorage'
                pure unit
      in
        R.div { className: "login", children:
          [ R.div { children: [ R.text "Login" ] }
          , R.form
            { onSubmit: handleOnSubmit
            , children:
              [ R.div
                { className: "form-row", children:
                  [ R.label
                    { htmlFor: "user"
                    , children: [ R.text "user" ]
                    }
                  , R.input
                    { type: "text"
                    , id: "user"
                    , required: true
                    , value: state.user
                    , onChange: handleOnChangeUser
                    }
                  ]
                }
              , R.div
                { className: "form-row", children:
                  [ R.label
                    { htmlFor: "pass"
                    , children: [ R.text "pass" ]
                    }
                  , R.input
                    { type: "text"
                    , id: "pass"
                    , required: true
                    , value: state.pass
                    , onChange: handleOnChangePass
                    }
                  ]
                }
              , R.div
                { className: "form-row", children:
                  [ R.input
                    { type: "submit"
                    , className: "login-btn"
                    , value: "Login"
                    }
                  ]
                }
              ]
            }
          ]
        }
