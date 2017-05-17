module Test.PullTogether.Feature.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Random (RANDOM)
import Node.ChildProcess (CHILD_PROCESS)
import Test.PullTogether.Feature.Env (ENV)

type Effects eff =
  ( cp :: CHILD_PROCESS
  , avar :: AVAR
  , random :: RANDOM
  , env :: ENV
  | eff
  )
