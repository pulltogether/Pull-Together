{-
Copyright 2015 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Feature.Monad where

import Prelude
import Selenium.Combinators as Combinators
import Control.Alt (alt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.State.Class (get, put)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple, fst, snd)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.Process (PROCESS)
import Platform (PLATFORM, getPlatform, runOs, runPlatform)
import Selenium.Key (metaKey, controlKey)
import Selenium.Monad (Selenium, attempt)
import Selenium.Types (ControlKey, Driver)

type FeatureEffects eff =
  ( platform ∷ PLATFORM
  , exception ∷ EXCEPTION
  , fs ∷ FS
  , process ∷ PROCESS
  , buffer ∷ BUFFER
  | eff)

type Person a = Tuple Driver a

type Feature eff a o = Selenium (FeatureEffects eff) ( person ∷ a | o )

as
  ∷ forall eff a o
  . ({ person ∷ a, driver ∷ Driver, defaultTimeout ∷ Milliseconds | o } → Person a)
  → Feature eff a o Unit
as f = do
  s ← get
  put $ s { person = snd $ f s, driver = fst $ f s }

getPlatformString ∷ forall eff a o. Feature eff a o String
getPlatformString = do
  platform ← getPlatform
  pure $ fromMaybe ""
    $ platform
    >>= runPlatform
    >>> _.os
    >>> runOs
    >>> _.family

getModifierKey ∷ forall eff a o. Feature eff a o ControlKey
getModifierKey = map modifierKey getPlatformString
  where
  modifierKey "Darwin" = metaKey
  modifierKey _ = controlKey

notMindingIfItsNotPossible ∷ forall eff a o. Feature eff a o Unit → Feature eff a o Unit
notMindingIfItsNotPossible = flip alt (pure unit)

await' ∷ forall eff a o. Milliseconds → String → Feature eff a o Boolean → Feature eff a o Unit
await' timeout msg check =
  attempt (Combinators.await timeout check)
    >>= either (const $ liftEff $ throw msg) (const $ pure unit)

-- | Same as `await'` but max wait time is setted to `config.selenium.waitTime`
await ∷ forall eff a o. String → Feature eff a o Boolean → Feature eff a o Unit
await msg check =
  get >>= \r → await' r.defaultTimeout msg check
