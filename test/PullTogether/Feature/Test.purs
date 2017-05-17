module Test.PullTogether.Feature.Test where

import Prelude

import Test.PullTogether.Feature.Monad (PTFeature)

test :: PTFeature Unit
test =
  pure unit