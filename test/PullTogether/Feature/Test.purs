module Test.PullTogether.Feature.Test where

import Prelude
import Test.PullTogether.Feature.Interactions as Interact
import Test.Feature.Monad (as)
import Test.Feature.Scenario (KnownIssueUrl, scenario)
import Test.PullTogether.Feature.Monad (PTFeature)

data Read = Read | Unread

pullTogetherScenario ∷ String → Array KnownIssueUrl → PTFeature Unit → PTFeature Unit
pullTogetherScenario =
  scenario
    "Pull Together"
    (pure unit)
    (pure unit)

test ∷ PTFeature Unit
test = do
  pullTogetherScenario "Start a pull together" [] do
    as _.amani
    
    Interact.accessPullTogether
    Interact.provideGroupName
    --Interact.signUp
    --Expect.noNotesMessage
    --Expect.newMessagesInPidgeonhole 0
    --Expect.groupName
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
    --Interact.signUp
    --Interact.inviteSomeone

    --as _.fatima
    --Interact.accessInvitationURL
    --Interact.signUp
    Interact.signOut
  
  pullTogetherScenario "postNoteWithWarnings a note" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.postNoteWithWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]
    --Interact.postNoteWithWarnings Note.happyNote []
    --Interact.invite _.nura

    --as _.nura
    --Interact.signUp
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
    --Interact.invite _.nura

    --as _.nura
    --Interact.signUp
    --Interact.addWarningToNote Note.depressionNote [ Warning.depression ]
    --Expect.noteObscuredByWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]
    --Interact.addWarningToNote Note.selfWorthNote [ Warning.selfWorth ]
    --Expect.noteObscuredByWarnings Note.selfWorthNote [ Warning.selfWorth ]
    Interact.signOut

  pullTogetherScenario "Check notes" [] do
    as _.amani
    --Interact.startAPullTogether
    --Interact.invite _.nura

    --as _.nura
    --Interact.signUp
    --Interact.postNoteWithWarnings Note.selfWorthNote [ Warning.selfWorth ]

    --as _.amani
    --Interact.postNoteWithWarnings Note.depressionNote [ Warning.mentalHealth, Warning.depression ]

    --as _.nura
    --Interact.postNoteWithWarnings Note.happyNote []
    --Interact.invite _.fatima

    --as _.fatima
    --Interact.signUp
    --Interact.acceptWarnings [ Warning.mentalHealth, Warning.depression, Warning.selfWorth ]
    --Expect.notesInOrder [ Note.selfWorthNote, Note.depressionNote, Note.happyNote ]
    Interact.signOut
  
  pullTogetherScenario "Messaging" [] do
    as _.amani
--    Interact.startAPullTogether
--    Interact.postNoteWithWarnings selfWorthNote [ "Self worth" ]
--    Interact.invite _.fatima
--
--    as _.fatima
--    Interact.signUp
--    Interact.unobscureFirstNoteWithWarnings [ "Self worth" ]
--    Interact.replyToNoteWithWarnings selfWorthNote sympatheticReply []
--    Expect.note selfWorthNote
--    Expect.messageFromMe empatheticReply
--    Interact.checkPidgeonhole
--    Expect.conversationsInOrder [ Conversation Read Note.selfWorthNote sympatheticReply ]
--    Interact.accessConversation $ Conversation Read Note.selfWorthNote sympatheticReply
--    Expect.note selfWorthNote
--    Expect.messageFromMe empatheticReply
--    Interact.checkNotes
--
--    as _.amani
--    Expect.newMessagesInPidgeonhole 1
--    Interact.invite _.nura
--
--    as _.nura
--    Interact.signUp
--    Interact.unobscureFirstNoteWithWarnings [ "Self worth" ]
--    Interact.replyToNoteWithWarnings selfWorthNote empatheticReply [ "Mental health" ]
--
--    as _.amani
--    Expect.newMessagesInPidgeonhole 2
--    Interact.checkPidgeonhole
--    Expect.newMessagesInPidgeonhole 0
--    Expect.conversationsInOrder
--      [ ObscuredConversation Unread Note.selfWorthNote [ Warning.mentalHealth ]
--      , Conversation Unread Note.selfWorthNote sympatheticReply
--      ]
--    Interact.accessConversation $ ObscuredConversation selfWorthNote [ "Mental health" ]
--    Ineract.unobscureFirstNoteWithWarnings [ "Self worth" ]
--    Expect.note selfWorthNote
--    Expect.unobscureFirstMessageWithWarnings [ "Mental health" ]
--    Expect.messageFromOther empatheticReply
--
--    as _.nura
--    Interact.replyToCurrentConversationWithWarnings reasuringReply []
--
--    as _.amani
--    Expect.newMessagesInPidgeonhole 0
--    Expect.messageFromOther reasuringReply
--
--    as _.fatima
--    Interact.replyToNoteWithWarnings selfWorthNote ableistReply []
--
--    as _.amani
--    Expect.newMessagesInPidgeonhole 1
--    Interact.replyToCurrentConversationWithWarnings thankfulReply [ "Mental health" ]
--    Interact.checkPidgeonhole
--    Expect.newMessagesInPidgeonhole 0
--    Expect.conversationsInOrder
--      [ Conversation Read Note.selfWorthNote thankfulReply
--      , Conversation Unread Note.selfWorthNote ableistReply
--      ]
--    Interact.accessConversation $ Conversation Note.selfWorthNote ableistReply
--    Expect.messageFromOther ableistReply
--    Interact.addWarningToReply ableistReply [ "Abeism" ]
--    Interact.replyToCurrentConversationWithWarnings explanationReply []
--
--    as _.fatima
--    Interact.unobscureFirstMessageWithWarnings [ "Ableism" ]
--    Interact.replyToCurrentConversationWithWarnings unhelpfulReply []
--
--    as _.amani
--    Expect.messageFromOther unhelpfulReply
--    Interact.ignoreCurrentConversation
--
--    as _.nura
--    Interact.replyToCurrentConversationWithWarnings friendlyReply []
--
--    as _.fatima
--    Interact.replyToCurrentConversationWithWarnings misguidedReply []
--
--    as _.amani
--    Expect.newMessagesInPidgeonhole 1
--    Interact.checkPidgeonhole
--    Expect.conversationsInOrder
--      [ Conversation Unread Note.selfWorthNote friendlyReply
--      , IgnoredConversation Unread Note.selfWorthNote
--      ]
    
-- Scenario: Change username / password
-- Scenario: Ignore warnings
-- Scenario: Timed warning acceptance
-- Scenario: Panic button
-- Scenario: Bookmarks
-- Scenario: Search
-- Scenario: Blocking
