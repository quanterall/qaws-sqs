{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SQS.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Network.AWS as AWS
import RIO

newtype QueueUrl = QueueUrl {unQueueUrl :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype WaitTime = WaitTime {unWaitTime :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MessageLimit = MessageLimit {unMessageLimit :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype ReceiptHandle = ReceiptHandle {unReceiptHandle :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MessageId = MessageId {unMessageId :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype ARN = ARN {unARN :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MessageCount = MessageCount {unMessageCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype DelayedMessageCount = DelayedMessageCount {unDelayedMessageCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype NotVisibleCount = NotVisibleCount {unNotVisibleCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A message that is sent to a queue. In contrast to the standard message type in the amazonka
-- libraries, this asserts that message id, receipt handle and body are all present.
data SQSMessage a = SQSMessage
  { _sqsMessageBody :: a,
    _sqsMessageMessageId :: MessageId,
    _sqsMessageReceiptHandle :: ReceiptHandle
  }
  deriving (Eq, Show)

makeLenses ''SQSMessage

data ReceiveMessageError
  = ReceiveMessageAWSError AWS.Error
  | ReceiveMessageDecodingError String
  | ReceiveMessageNoBody
  | ReceiveMessageNoReceiptHandle
  | ReceiveMessageNoMessageId
  deriving (Show)

instance Exception ReceiveMessageError

data QueueAttributes = QueueAttributes
  { queueAttributesArn :: !(Maybe Text),
    queueAttributesUrl :: !QueueUrl,
    queueAttributesMessages :: !(Maybe MessageCount),
    queueAttributesDelayedMessages :: !(Maybe DelayedMessageCount),
    queueAttributesNotVisibleMessages :: !(Maybe NotVisibleCount)
  }
  deriving (Eq, Show)
