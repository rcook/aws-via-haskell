--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to expanded automatically
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- All imports are explicit so we can see exactly where each function comes from
import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), set)
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.AWS hiding (await)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text
import           Network.AWS (Service, await)
import           Network.AWS.DynamoDB
                    ( _ResourceInUseException
                    , _ResourceNotFoundException
                    , KeyType(..)
                    , ScalarAttributeType(..)
                    , attributeDefinition
                    , attributeValue
                    , avN
                    , avS
                    , createTable
                    , ctAttributeDefinitions
                    , deleteTable
                    , describeTable
                    , dynamoDB
                    , getItem
                    , giKey
                    , girsItem
                    , keySchemaElement
                    , piItem
                    , provisionedThroughput
                    , putItem
                    , tableExists
                    , tableNotExists
                    )
import           System.IO (stdout)

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn

data DynamoDBEnv = DynamoDBEnv
    { env :: Env
    , service :: Service
    , region :: Region
    , tableName :: Text
    }

-- Creates a table in DynamoDB and waits until table is in active state
-- Demonstrates:
-- * Use of runResourceT, runAWST
-- * Use of reconfigure
-- * How to handle exceptions in lenses
-- * Basic use of amazonka-style lenses
-- * How to wait on an asynchronous operation
doCreateTableIfNotExists :: DynamoDBEnv -> IO ()
doCreateTableIfNotExists DynamoDBEnv{..} = do
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            exists <- handling _ResourceInUseException (const (pure True)) $ do
                void $ send $ createTable
                    tableName
                    (keySchemaElement "counter_name" Hash :| [])
                    (provisionedThroughput 5 5)
                    & ctAttributeDefinitions .~ [ attributeDefinition "counter_name" S ]
                return False
            when (not exists) (void $ await tableExists (describeTable tableName))

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: DynamoDBEnv -> IO ()
doDeleteTableIfExists DynamoDBEnv{..} = do
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            exists <- handling _ResourceNotFoundException (const (pure False)) $ do
                void $ send $ deleteTable tableName
                return True
            when exists (void $ await tableNotExists (describeTable tableName))

-- Puts an item into the DynamoDB table
doPutItem :: DynamoDBEnv -> IO ()
doPutItem DynamoDBEnv{..} = do
    let item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "foo")
            , ("counter_value", attributeValue & avN .~ Just "1001")
            ]
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            void $ send $ putItem tableName & piItem .~ item

-- Gets an item from the DynamoDB table
doGetItem :: DynamoDBEnv -> IO ()
doGetItem DynamoDBEnv{..} = do
    let key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "foo")
            ]
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            result <- send $ getItem tableName & giKey .~ key
            void $ case HashMap.lookup "counter_value" (result ^. girsItem) of
                Nothing -> say "No counter_value field"
                Just value -> case value ^. avN of
                            Nothing -> say "Invalid counter_value"
                            Just valueN -> say $ "Value: " <> (Text.pack $ show valueN)

main :: IO ()
main = do
    -- Environment that obtains credentials using the standard algorithm and provides no logging
    --env <- newEnv Discover

    -- Environment that obtains credentials using the standard algorithm and provides debug logging
    env <- do
        logger <- newLogger Debug stdout
        newEnv Discover <&> set envLogger logger

    -- Use this to run against a local DynamoDB instance
    let service = setEndpoint False "localhost" 8000 dynamoDB
    -- Use this to run against a DynamoDB instance running on AWS
    --let db = dynamoDB

    let dynamoDBEnv = DynamoDBEnv env service Ohio "BAR"

    putStrLn "DeleteTable"
    doDeleteTableIfExists dynamoDBEnv

    putStrLn "CreateTable"
    doCreateTableIfNotExists dynamoDBEnv

    putStrLn "PutItem"
    doPutItem dynamoDBEnv

    putStrLn "GetItem"
    doGetItem dynamoDBEnv

    putStrLn "Done"
