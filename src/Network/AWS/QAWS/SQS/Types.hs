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

newtype ReceiptHandle = ReceiptHandle {unReceiptHandle :: Text}
  deriving (Eq, Show)

newtype MessageId = MessageId {unMessageId :: Text}
  deriving (Eq, Show)

data SQSMessage a = SQSMessage
  { _sqsMessageBody :: a,
    _sqsMessageMessageId :: MessageId,
    _sqsMessageReceiptHandle :: ReceiptHandle
  }
  deriving (Eq, Show)

makeLenses ''SQSMessage
