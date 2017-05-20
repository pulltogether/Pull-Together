module Test.PullTogether.Feature.Interactions where

import Prelude

import Test.Feature as Feature
import XPath as XPath

import Selenium.Monad (get)
import Test.PullTogether.Feature.Monad (PTFeature, getConfig)

accessPullTogether ∷ PTFeature Unit
accessPullTogether = get <<< _.ptUrl =<< getConfig

confirm ∷ PTFeature Unit
confirm =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "OK"

signOut ∷ PTFeature Unit
signOut =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Sign out"