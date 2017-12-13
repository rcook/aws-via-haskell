--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

defaultRegion :: Region
defaultRegion = Ohio

tableName :: Text
tableName = "BAR"

-- Creates a table in DynamoDB and waits until table is in active state
doCreateTableIfNotExists :: Env -> Service -> IO ()
doCreateTableIfNotExists env db = do
    runResourceT . runAWST env . within defaultRegion $ do
        reconfigure db $ do
            exists <- handling _ResourceInUseException (const (pure True)) $ do
                void $ send $ createTable
                    tableName
                    (keySchemaElement "counter_name" Hash :| [])
                    (provisionedThroughput 5 5)
                    & ctAttributeDefinitions .~ [attributeDefinition "counter_name" S]
                return False
            when (not exists) (void $ await tableExists (describeTable tableName))

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: Env -> Service -> IO ()
doDeleteTableIfExists env db = do
    runResourceT . runAWST env . within defaultRegion $ do
        reconfigure db $ do
            exists <- handling _ResourceNotFoundException (const (pure False)) $ do
                void $ send $ deleteTable tableName
                return True
            when exists (void $ await tableNotExists (describeTable tableName))

-- Puts an item into the DynamoDB table
doPutItem :: Env -> Service -> IO ()
doPutItem env db = do
    let item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "foo")
            , ("counter_value", attributeValue & avN .~ Just "1001")
            ]
    runResourceT . runAWST env . within defaultRegion $ do
        reconfigure db $ do
            void $ send $ putItem tableName & piItem .~ item

-- Gets an item from the DynamoDB table
doGetItem :: Env -> Service -> IO ()
doGetItem env db = do
    let key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "foo")
            ]
    runResourceT . runAWST env . within defaultRegion $ do
        reconfigure db $ do
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
    let db = setEndpoint False "localhost" 8000 dynamoDB
    -- Use this to run against a DynamoDB instance running on AWS
    --let db = dynamoDB

    putStrLn "DeleteTable"
    doDeleteTableIfExists env db

    putStrLn "CreateTable"
    doCreateTableIfNotExists env db

    putStrLn "PutItem"
    doPutItem env db

    putStrLn "GetItem"
    doGetItem env db

    putStrLn "Done"
