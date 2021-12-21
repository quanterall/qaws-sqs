module Network.AWS.QAWS.SQS where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), eitherDecodeStrict)
import qualified Network.AWS as AWS
import Network.AWS.QAWS
import Network.AWS.QAWS.SQS.Types
import qualified Network.AWS.SQS as AWSSQS
import RIO

data ReceiveMessageError
  = ReceiveMessageAWSError AWS.Error
  | ReceiveMessageDecodingError String
  | ReceiveMessageNoBody
  | ReceiveMessageNoReceiptHandle
  | ReceiveMessageNoMessageId
  deriving (Show)

instance Exception ReceiveMessageError

receiveMessages ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either AWS.Error [AWSSQS.Message])
receiveMessages queueUrl waitTime messageLimit = do
  awsEnv <- view AWS.environment
  receiveMessages' awsEnv queueUrl waitTime messageLimit

receiveWithPayload ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env, FromJSON a) =>
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either ReceiveMessageError [SQSMessage a])
receiveWithPayload queueUrl waitTime messageLimit = do
  awsEnv <- view AWS.environment
  receiveWithPayload' awsEnv queueUrl waitTime messageLimit

receiveWithPayload' ::
  (MonadUnliftIO m, FromJSON a) =>
  AWS.Env ->
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either ReceiveMessageError [SQSMessage a])
receiveWithPayload' awsEnv queueUrl waitTime messageLimit = do
  commandResult <-
    mapLeft ReceiveMessageAWSError
      <$> receiveMessages' awsEnv queueUrl waitTime messageLimit
  pure $ either Left (mapM decodeMessage) commandResult
  where
    decodeMessage :: (FromJSON a) => AWSSQS.Message -> Either ReceiveMessageError (SQSMessage a)
    decodeMessage m = do
      body <- note ReceiveMessageNoBody $ m ^. AWSSQS.mBody
      _sqsMessageReceiptHandle <- note ReceiveMessageNoReceiptHandle $ m ^. AWSSQS.mReceiptHandle
      _sqsMessageMessageId <- note ReceiveMessageNoMessageId $ m ^. AWSSQS.mMessageId
      let bytes = encodeUtf8 body
      _sqsMessageBody <- mapLeft ReceiveMessageDecodingError $ eitherDecodeStrict bytes
      pure $ SQSMessage {_sqsMessageBody, _sqsMessageReceiptHandle, _sqsMessageMessageId}

receiveMessages' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  QueueUrl ->
  WaitTime ->
  MessageLimit ->
  m (Either AWS.Error [AWSSQS.Message])
receiveMessages' awsEnv (QueueUrl queueUrl) (WaitTime waitTime) (MessageLimit messageLimit) = do
  let command =
        AWSSQS.receiveMessage queueUrl
          & AWSSQS.rmWaitTimeSeconds ?~ waitTime
          & AWSSQS.rmMaxNumberOfMessages ?~ messageLimit
  commandResult <- tryRunAWS' awsEnv command
  either (Left >>> pure) ((^. AWSSQS.rmrsMessages) >>> Right >>> pure) commandResult

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right
