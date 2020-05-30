module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Thermite as T

import React as R
import React.DOM as R
import React.DOM.Props as RP

data Action = Increment | Decrement

type State = { counter :: Int }

initialState :: State
initialState = { counter: 0 }

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.p' [ R.text "Value: "
         , R.text $ show state.counter
         ]
  , R.p' [ R.button [ RP.onClick \_ -> dispatch Increment ]
                    [ R.text "Increment" ]
         , R.button [ RP.onClick \_ -> dispatch Decrement ]
                    [ R.text "Decrement" ]
         ]
  ]

performAction :: T.PerformAction State _ Action
performAction Increment _ _ = void (T.cotransform (\state -> state { counter = state.counter + 1 }))
performAction Decrement _ _ = void (T.cotransform (\state -> state { counter = state.counter - 1 }))

main :: Effect Unit
main = do
  log "🍝"
