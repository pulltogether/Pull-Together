module Test.PullTogether.Feature.Expectations where

import Prelude

import XPath as XPath
import Test.Feature as Feature
import Test.PullTogether.Feature.Monad (PTFeature)

signOutOptionPresented ∷ PTFeature Unit
signOutOptionPresented =
  Feature.expectPresented $ XPath.anywhere $ XPath.anyWithExactText "Sign out"