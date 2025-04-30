{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}

module StateTransition where

import DFCT.Provenance ()
import DFCT.Types hiding (Contribution)
import qualified PlutusTx.Prelude as PlutusTx hiding (Semigroup(..), unless)
import qualified PlutusTx ()
import qualified Data.ByteString.Char8 as BS
import Test.Tasty.HUnit

import TestFixtures

-- State Transitions
testStateTransitionInvalid :: Assertion
testStateTransitionInvalid =
    mkStateTransitionTest
        TopicProposed
        TopicActivated
        (TopicAction (ActivateTopic (PlutusTx.toBuiltin (BS.pack "topic1"))))
        [defaultProposer]
        False

testStateTransitionValid :: Assertion
testStateTransitionValid =
    mkStateTransitionTest
        TopicReviewed
        TopicActivated
        (TopicAction (ActivateTopic (PlutusTx.toBuiltin (BS.pack "topic1"))))
        [defaultProposer]
        True