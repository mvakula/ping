module Main where

import Prelude

import React.Basic (ReactComponent, react)
import React.Basic.DOM as R

main :: ReactComponent {}
main = react { displayName: "Main", initialState, receiveProps, render }
  where
    initialState = {}
    receiveProps _ _ _ = pure unit
    render _ state setState =
      R.div { children: [ R.text "Hello World" ]}
