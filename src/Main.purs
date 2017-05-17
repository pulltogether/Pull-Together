module Main where

import Prelude

import PullTogether.Component as PT
import Halogen.Aff as HA

import Control.Monad.Eff (Eff)
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI PT.comp unit body