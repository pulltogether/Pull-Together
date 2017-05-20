module Test.PullTogether.Feature.Test where

import Prelude
import Test.PullTogether.Feature.Interactions as Interact
import Selenium.Monad (get)
import Test.Feature (accessUrlFromFieldValueAs)
import Test.Feature.Monad (as)
import Test.Feature.Scenario (KnownIssueUrl, scenario)
import Test.PullTogether.Feature.Monad (PTFeature)

pullTogetherScenario ∷ String → Array KnownIssueUrl → PTFeature Unit → PTFeature Unit
pullTogetherScenario =
  scenario
    "Pull Together"
    (pure unit)
    (pure unit)

test ∷ PTFeature Unit
test = do
  pullTogetherScenario "Test CMUFT" [] do
    as _.amani
    get "data:text/html;base64,PGlucHV0IHZhbHVlPSJodHRwOi8vZ29vZ2xlLmNvbSI+"
    accessUrlFromFieldValueAs _.nura "//input"

  pullTogetherScenario "Start a pull together" [] do
    as _.amani
    --Interact.accessPullTogether
    --Interact.provideGroupName
    --Interact.providePassword
    --Expect.noNotesMessage
    --Expect.messagesInPidgeonhole 0
    Interact.signOut
    
  pullTogetherScenario "Sign in" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.postNoteWithWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]
    --Interact.inviteSomeone
    --Interact.checkPidgeonhole
    Interact.signOut

  pullTogetherScenario "Invite people to pull together" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.invite _.nura

    --as _.nura
    --Interact.providePassword
    --Interact.inviteSomeone

    --as _.fatima
    --Interact.accessInvitationURL
    --Interact.providePassword
    Interact.signOut
  
  pullTogetherScenario "postNoteWithWarnings a note" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.postNoteWithWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]
    --Interact.postNoteWithWarnings Note.happyNote []
    --Interact.invite _.nura

    --as _.nura
    --Interact.providePassword
    --Interact.postNoteWithWarnings Note.selfWorthNote []
    --Expect.note Note.happyNote
    --Expect.note Note.selfWorthNote
    --Expect.noteObscuredByWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]
    --Interact.acceptWarnings [ Warning.mentalHealth, Warning.depression ]
    --Expect.notesInOrder [ Note.depressionNote, Note.happyNote, Note.selfWorthNote ]
    --Interact.replyToNoteWithWarnings Note.depressionNote Reply.sympatheticReply []
    --Interact.addWarningToNote Note.selfWorthNote Warning.selfWorth
    --Interact.acceptWarnings [ Warning.selfWorth ]
    --Interact.obscureNoteBehindWarnings Note.selfWorthNote
    Interact.signOut
  
  pullTogetherScenario "Add warnings to notes" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.postNoteWithWarnings Note.depressionNote [ Warning.mentalHealth ]
    --Interact.postNoteWithWarnings Note.selfWorthNote []

    --as _.nura
    --Interact.addWarningToNote Note.depressionNote [ Warning.depression ]
    --Expect.noteObscuredByWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]
    --Interact.addWarningToNote Note.selfWorthNote [ Warning.selfWorth ]
    --Expect.noteObscuredByWarnings Note.selfWorthNote [ Warning.selfWorth ]
    Interact.signOut

  pullTogetherScenario "Check notes" [] do
    --as _.amani
    --Interact.startAPullTogether

    --as _.nura
    --Interact.postNoteWithWarnings Note.selfWorthNote [ Warning.selfWorth ]

    --as _.amani
    --Interact.postNoteWithWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]

    --as _.nura
    --Interact.postNoteWithWarnings Note.happyNote []

    --as _.fatima
    --Interact.acceptWarnings [ Warning.mentalHealth, Warning.depression, Warning.selfWorth ]
    --Expect.notesInOrder [ Note.selfWorthNote, Note.depressionNote, Note.happyNote ]
    Interact.signOut
  
  pullTogetherScenario "Reply" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.postNoteWithWarnings Note.selfWorthNote [ Warning.selfWorth ]

    --as _.fatima
    --Interact.acceptWarnings [ Warning.selfWorth ]
    --Interact.replyToNoteWithWarnings Note.selfWorthNote Reply.sympatheticReply []

    --as _.amani
    --Interact.checkNotes
    --Expect.messagesInPidgeonhole 1

    --as _.nura
    --Interact.acceptWarnings [ Warning.selfWorth ]
    --Interact.replyToNoteWithWarnings Note.selfWorthNote Reply.empatheticReply [ Warning.selfWorth ]

    --as _.amani
    --Interact.checkNotes
    --Expect.messagesInPidgeonhole 2
    --Interact.checkPidgeonhole
    --Expect.messagesInPidgeonhole 0
    --Expect.reply Note.selfWorthNote [ Reply.sympatheticReply ]
    --Interact.acceptWarnings [ Warning.selfWorth ]
    --Expect.reply Note.selfWorthNote [ Reply.empatheticReply ]

