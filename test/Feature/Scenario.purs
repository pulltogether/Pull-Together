module Test.Feature.Scenario where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message, throw)
import Data.Either (Either(..))
import Data.String (joinWith)
import Test.Feature.Log (sectionMsg, warnMsg)
import Test.Feature (logCurrentScreen)
import Test.Feature.Monad (Feature)
import Selenium.Monad (attempt)

type EpicTitle = String
type Hook eff b o = Feature eff b o
type ScenarioTitle = String
type KnownIssueUrl = String

scenario
  ∷ forall eff b o
  . EpicTitle
  → Hook eff b o Unit
  → Hook eff b o Unit
  → ScenarioTitle
  → Array KnownIssueUrl
  → Feature eff b o Unit
  → Feature eff b o Unit
scenario epic before after title knownIssues actions =
  sectionMsg title' *> runHook before *> actions'
  where
  title' ∷ String
  title' = epic <> ": " <> title

  knownIssuesString ∷ String
  knownIssuesString = separate $ indent <$> knownIssues

  knownIssuesWarning ∷ String
  knownIssuesWarning = "These known issues caused this scenario to fail:\n" <> knownIssuesString

  knownIssuesHookWarning = "These known issues caused this scenario hook to fail:\n" <> knownIssuesString

  separate ∷ Array String → String
  separate = joinWith "\n"

  indent ∷ String → String
  indent s = "  " <> s

  warning ∷ String → String
  warning s = "Warning: " <> s

  warn ∷ String → Feature eff b o Unit
  warn = warnMsg <<< warning

  unexpectedSuccess ∷ Feature eff b o Unit
  unexpectedSuccess =
    warn
      $ "Ok despite known issues, if these issues are resolved please remove them\n"
      <> knownIssuesString

  actions' ∷ Feature eff b o Unit
  actions' | knownIssues == [] =
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → runHook after
  actions' =
    attempt actions >>=
      case _ of
        Left e → logCurrentScreen *> warn (message e) *> warn knownIssuesWarning *> runHook after
        Right _ → runHook after *> unexpectedSuccess

  runHook ∷ Feature eff b o Unit → Feature eff b o Unit
  runHook hook | knownIssues == [] =
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> (liftEff $ throw $ message e)
        Right _ → pure unit
  runHook hook =
    attempt hook >>=
      case _ of
        Left e → logCurrentScreen *> warn (message e) *> warn knownIssuesHookWarning
        Right _ → pure unit
