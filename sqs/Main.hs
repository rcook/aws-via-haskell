--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Config(..)
                    , LoggingState(..)
                    , ServiceClass(..)
                    , ServiceEndpoint(..)
                    , SessionClass(..)
                    , Session
                    , connect
                    , withAWS
                    )
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.))
import           Control.Monad (forM_, void)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Network.AWS
                    ( Credentials(..)
                    , Service
                    , send
                    )
import           Network.AWS.SQS
                    ( _QueueDoesNotExist
                    , createQueue
                    , getQueueURL
                    , gqursQueueURL
                    , listQueues
                    , lqrsQueueURLs
                    , mBody
                    , receiveMessage
                    , rmrsMessages
                    , sendMessage
                    , sqs
                    )

data SQSService = SQSService Service

instance ServiceClass SQSService where
    type TypedSession SQSService = SQSSession
    rawService (SQSService raw) = raw
    wrappedSession = SQSSession

data SQSSession = SQSSession Session

instance SessionClass SQSSession where
    rawSession (SQSSession raw) = raw

newtype QueueName = QueueName Text deriving Show

newtype QueueURL = QueueURL Text deriving Show

sqsService :: SQSService
sqsService = SQSService sqs

doListQueues :: SQSSession -> IO [Text]
doListQueues = withAWS $ do
    result <- send $ listQueues
    return $ result ^. lqrsQueueURLs

doCreateQueue :: QueueName -> SQSSession -> IO ()
doCreateQueue (QueueName qn) = withAWS (void $ send $ createQueue qn)

doGetQueueURL :: QueueName -> SQSSession -> IO (Maybe QueueURL)
doGetQueueURL (QueueName qn) = withAWS $ do
    handling _QueueDoesNotExist (const (pure Nothing)) $ do
        result <- send $ getQueueURL qn
        return $ Just (QueueURL $ result ^. gqursQueueURL)

doSendMessage :: QueueURL -> Text -> SQSSession -> IO ()
doSendMessage (QueueURL s) m = withAWS $ do
    void $ send $ sendMessage s m

doReceiveMessage :: QueueURL -> SQSSession -> IO (Maybe Text)
doReceiveMessage (QueueURL s) = withAWS $ do
    result <- send $ receiveMessage s
    case result ^. rmrsMessages of
        m : [] -> return $ m ^. mBody
        _ -> return Nothing

main :: IO ()
main = do
    let queueName = QueueName "my-queue"

    sqsSession <- connect
                    (Config (Local "localhost" 4576) LoggingDisabled Discover)
                    sqsService

    putStrLn "CreateQueue"
    doCreateQueue queueName sqsSession

    putStrLn "ListQueues"
    queueURLs <- doListQueues sqsSession
    forM_ queueURLs $ \queueURL ->
        Text.putStrLn $ "  " <> queueURL

    putStrLn "GetQueueURL"
    mbQueueURL <- doGetQueueURL queueName sqsSession
    case mbQueueURL of
        Nothing -> Text.putStrLn "  (not found)"
        Just queueURL -> do
            putStrLn $ "  " <> show queueURL

            putStrLn "SendMessage"
            doSendMessage queueURL "a message" sqsSession

            putStrLn "ReceiveMessage"
            m <- doReceiveMessage queueURL sqsSession
            putStrLn $ "  " <> show m
