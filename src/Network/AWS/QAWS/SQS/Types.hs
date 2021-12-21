{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SQS.Types where

import Control.Lens.TH (makeLenses)
import RIO

newtype QueueUrl = QueueUrl {unQueueUrl :: Text}
  deriving (Eq, Show)

newtype WaitTime = WaitTime {unWaitTime :: Int}
  deriving (Eq, Show)

newtype MessageLimit = MessageLimit {unMessageLimit :: Int}
  deriving (Eq, Show)

data SQSMessage a = SQSMessage
  { _sqsMessageBody :: a,
    _sqsMessageMessageId :: Text,
    _sqsMessageReceiptHandle :: Text
  }
  deriving (Eq, Show)

makeLenses ''SQSMessage
