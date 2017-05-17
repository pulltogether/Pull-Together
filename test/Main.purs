module Test.Main where

import Prelude
import Test.PullTogether.Feature.Main as Feature
import Test.PullTogether.Property.Main as Property
import Control.Monad.Eff (Eff)
import Test.PullTogether.Feature.Main (Effects)

main :: Eff Effects Unit
main = do
  Property.main
  Feature.main
