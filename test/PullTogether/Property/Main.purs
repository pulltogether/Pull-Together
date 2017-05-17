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

module Test.PullTogether.Property.Main where

import Prelude

import Control.Monad.Eff.Console (log)

import Test.StrongCheck (SC)
--import Test.StrongCheck as SC

main ∷ ∀ eff. SC eff Unit
main = do
  log "No property tests yet"
  -- Example to i
  --SC.quickCheck' 1000 \thing →
  --  case X.decode (X.encode thing) of
  --    Left err →
  --      SC.Failed $ "Decode failed: " <> err
  --    Right thing' →
  --      thing == thing'
  --        <?> ("model mismatch:\n "
  --           <> show (J.encodeJson model)
  --           <> "\n" <> show (J.encodeJson model'))
