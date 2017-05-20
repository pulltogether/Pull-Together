module Test.PullTogether.Feature.Main where

import Prelude

import Control.Monad.Aff (Aff, runAff, apathize, attempt)
import Control.Monad.Aff.AVar (makeVar, takeVar, putVar, AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Reattempt (reattempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Ec
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Eff.Ref (REF, newRef, readRef)
import Control.Monad.State.Trans (runStateT)
import Control.Monad.Error.Class (throwError)

import Data.Int as Int
import Data.Posix.Signal (Signal(SIGTERM))
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe)
import Data.Foldable (traverse_)

import DOM (DOM)

import Node.ChildProcess as CP
import Node.FS (FS)
import Node.FS.Aff (mkdir, unlink)
import Node.Path (resolve)
import Node.Process as Process
import Node.Rimraf (rimraf)
import Node.Stream (Readable, Duplex, pipe, onClose)

import Selenium (quit)
import Selenium.Browser (Browser(..), browserCapabilities)
import Selenium.Builder (withCapabilities, build)
import Selenium.Monad (setWindowSize)
import Selenium.Types (SELENIUM)

import Test.Feature.Monad (FeatureEffects)
import Test.PullTogether.Feature.Config (Config)
import Test.PullTogether.Feature.Effects as PTFEffects
import Test.PullTogether.Feature.Test (test)
import Test.PullTogether.Feature.Starter (starter)
import Text.Chalky (green, yellow, red)

foreign import createReadStream
  ∷ ∀ e. String → Aff e (Readable () e)
foreign import createWriteStream
  ∷ ∀ e. String → Aff e (Duplex e)
foreign import stack
  ∷ Error → String

type Effects =
  PTFEffects.Effects (FeatureEffects
    ( ref ∷ REF
    , console ∷ Ec.CONSOLE
    , dom ∷ DOM
    , selenium ∷ SELENIUM
    ))

runTests ∷ Config → Aff Effects Unit
runTests config = do
  driver1 ← build $ withCapabilities $ browserCapabilities Chrome
  driver2 ← build $ withCapabilities $ browserCapabilities Chrome
  driver3 ← build $ withCapabilities $ browserCapabilities Chrome

  let fatima = Tuple driver1 { name: "Fatima", password: "TAfgF98HVA" }
  let amani = Tuple driver2 { name: "Amani", password: "HpJ4SFXhOt" }
  let nura = Tuple driver3 { name: "Nura", password: "NhE1xDX5L1" }

  let
    defaultTimeout = Milliseconds $ Int.toNumber config.selenium.waitTime
    readerInp =
      { config
      , defaultTimeout
      , person: snd fatima
      , driver: fst fatima 
      , fatima
      , nura
      , amani
      }

  res ← attempt $ flip runStateT readerInp do
    setWindowSize { height: 800, width: 1024 }
    test

  apathize $ quit driver1
  apathize $ quit driver2
  apathize $ quit driver3
  either throwError (const $ pure unit) res

copyFile
  ∷ ∀ e
  . String
  → String
  → Aff (fs ∷ FS, avar ∷ AVAR, exception ∷ EXCEPTION|e) Unit
copyFile source tgt = do
  apathize $ unlink to
  readFrom ← createReadStream from
  writeTo ← createWriteStream to
  knot ← makeVar

  liftEff
    $ onClose writeTo
    $ void
    $ runAff
        (const $ pure unit)
        (const $ pure unit)
        (putVar knot unit)

  _ ← liftEff $ readFrom `pipe` writeTo
  takeVar knot
  where
  from = resolve [source] ""
  to = resolve [tgt] ""

procStartMaxTimeout ∷ Milliseconds
procStartMaxTimeout = Milliseconds 60000.0

startProc
  ∷ String
  → String
  → Array String
  → (Either String String → Maybe (Either String Unit))
  → Aff Effects CP.ChildProcess
startProc name command args check
  = reattempt procStartMaxTimeout
  $ starter name check
  $ liftEff
  $ CP.spawn command args CP.defaultSpawnOptions

cleanMkDir ∷ String → Aff Effects Unit
cleanMkDir path = do
  let p = resolve [path] ""
  rimraf p
  mkdir p

main ∷ Eff Effects Unit
main = do
  procs ← newRef []

  Process.onExit \_ → readRef procs >>= traverse_ (CP.kill SIGTERM)

  let rawConfig = { ptUrl: "http://localhost:1337", selenium: { waitTime: 15000 } }

  void $ runAff errHandler (const $ Process.exit 0) do
    -- Set up folders
    --log $ gray "Creating data folder for MongoDB"
    --cleanMkDir "tmp/data"
    --log $ gray "Emptying test folder"
    --cleanMkDir "tmp/test"
    --cleanMkDir "tmp/test/image"
    --cleanMkDir "tmp/test/downloads"
    --copyFile
    --  "test/quasar-config.json"
    --  "tmp/test/quasar-config.json"

    -- Spawn database
    --mongo ←
    --  startProc
    --    "MongoDB"
    --    "mongod"
    --    (mongoArgs rawConfig)
    --    (expectStdOut "waiting for connections on port")
    --liftEff $ modifyRef procs (Arr.cons mongo)

    log $ yellow "Starting tests"
    testResults
      ← attempt
        $ runTests rawConfig
    case testResults of
      Left e →  throwError e
      Right _ → log $ green "OK, tests passed."

  where
  errHandler e = do
    Ec.log $ red $ message e
    traverse_ (Ec.log <<< red) $ S.split (S.Pattern "\n") $ stack e
    Process.exit 1
