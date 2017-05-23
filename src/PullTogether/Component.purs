module PullTogether.Component where

import Prelude

import Halogen as H
import Halogen.HTML as HH
--import Halogen.HTML.Events as HE
--import Halogen.HTML.Properties as HP

import Data.Maybe (Maybe(..))

type State = Unit

data Query a
  = NoOp a

data Message = Toggled Boolean

comp :: forall m. H.Component HH.HTML Query Unit Message m
comp =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = unit

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Pull together" ]
      , HH.button_ [ HH.text "Sign out" ]
      , HH.label_
          [ HH.text "Group name"
          , HH.input []
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    NoOp next -> 
      pure next