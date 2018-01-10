{-|
Module      : Main
Description : Simple Queue Service demo
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Endpoint(..)
                    , awsConfig
                    , connect
                    , withAWS
                    , wrapAWSService
                    )
import           AWSViaHaskell.AWSPrelude
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.))
import           Control.Monad (forM_, void)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
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

wrapAWSService 'sqs "SQSService" "SQSSession"

newtype QueueName = QueueName Text deriving Show

newtype QueueURL = QueueURL Text deriving Show

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
                    (awsConfig (Local "localhost" 4576))
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
