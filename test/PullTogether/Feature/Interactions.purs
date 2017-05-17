module Test.PullTogether.Feature.Interactions where

import Prelude

import Test.Feature as Feature
import XPath as XPath

import Selenium.Monad (get)
import Test.PullTogether.Feature.Monad (PTFeature, getConfig)

launch ∷ PTFeature Unit
launch = get <<< _.ptUrl =<< getConfig

confirm ∷ PTFeature Unit
confirm =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "OK"