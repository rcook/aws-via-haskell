--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- All imports are explicit so we can see exactly where each function comes from
import           AWSViaHaskell
                    ( AWSInfo(..)
                    , LoggingState(..)
                    , withAWS
                    )
import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), set)
import           Control.Monad (void, when)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , Credentials(..)
                    , Env
                    , LogLevel(..)
                    , Region(..)
                    , envLogger
                    , newEnv
                    , newLogger
                    , send
                    , setEndpoint
                    )
import           Control.Monad.Trans.Resource (MonadBaseControl, ResourceT)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack)
import           Data.Text.Read (decimal)
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
                    , uiExpressionAttributeValues
                    , uiKey
                    , uiUpdateExpression
                    , updateItem
                    )
import           System.IO (stdout)

type HostName = ByteString

type Port = Int

data ServiceType = AWS Region | Local HostName Port

data DBInfo = DBInfo
    { env :: Env
    , service :: Service
    , region :: Region
    , tableName :: Text
    }

intToText :: Int -> Text
intToText = Text.pack . show

parseInt :: Text -> Maybe Int
parseInt s = case decimal s of
    Left _ -> Nothing
    Right (result, s') -> if Text.null s' then Just result else Nothing

getDBInfo :: LoggingState -> ServiceType -> IO DBInfo
getDBInfo loggingState serviceType = do
    env <- getEnv loggingState
    let (service, region) = serviceRegion serviceType
    return $ DBInfo env service region "table"
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

        -- Run against a DynamoDB instance running on AWS in specified region
        serviceRegion (AWS region) = (dynamoDB, region)
        -- Run against a local DynamoDB instance on a given host and port
        serviceRegion (Local hostName port) = (setEndpoint False hostName port dynamoDB, NorthVirginia)

withDynamoDB :: MonadBaseControl IO m =>
    Env
    -> Service
    -> Region
    -> AWST' Env (ResourceT m) a
    -> m a
withDynamoDB env service region action =
    let aws = AWSInfo env region service in withAWS aws action

-- Creates a table in DynamoDB and waits until table is in active state
-- Demonstrates:
-- * Use of runResourceT, runAWST
-- * Use of reconfigure
-- * How to handle exceptions in lenses
-- * Basic use of amazonka-style lenses
-- * How to wait on an asynchronous operation
doCreateTableIfNotExists :: DBInfo -> IO ()
doCreateTableIfNotExists DBInfo{..} = withDynamoDB env service region $ do
    exists <- handling _ResourceInUseException (const (pure True)) $ do
        void $ send $ createTable
            tableName
            (keySchemaElement "counter_name" Hash :| [])
            (provisionedThroughput 5 5)
            & ctAttributeDefinitions .~ [ attributeDefinition "counter_name" S ]
        return False
    when (not exists) (void $ await tableExists (describeTable tableName))

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: DBInfo -> IO ()
doDeleteTableIfExists DBInfo{..} = withDynamoDB env service region $ do
    exists <- handling _ResourceNotFoundException (const (pure False)) $ do
        void $ send $ deleteTable tableName
        return True
    when exists (void $ await tableNotExists (describeTable tableName))

-- Puts an item into the DynamoDB table
doPutItem :: DBInfo -> Int -> IO ()
doPutItem DBInfo{..} value = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            , ("counter_value", attributeValue & avN .~ Just (intToText value))
            ]

-- Updates an item in the DynamoDB table
doUpdateItem :: DBInfo -> IO ()
doUpdateItem DBInfo{..} = withDynamoDB env service region $ do
    void $ send $ updateItem tableName
        & uiKey .~ key
        & uiUpdateExpression .~ Just "ADD counter_value :increment"
        & uiExpressionAttributeValues .~ exprAttrValues
    where
        key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            ]
        exprAttrValues = HashMap.fromList
            [ (":increment", attributeValue & avN .~ Just "1" )
            ]

-- Gets an item from the DynamoDB table
doGetItem :: DBInfo -> IO (Maybe Int)
doGetItem DBInfo{..} = withDynamoDB env service region $ do
    result <- send $ getItem tableName
        & giKey .~ key
    return $ do
        valueAttr <- HashMap.lookup "counter_value" (result ^. girsItem)
        valueNStr <- valueAttr ^. avN
        parseInt valueNStr
    where key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            ]

main :: IO ()
main = do
    --db <- getDBInfo LoggingEnabled (AWS Ohio)
    db <- getDBInfo LoggingDisabled (Local "localhost" 8000)

    putStrLn "DeleteTable"
    doDeleteTableIfExists db

    putStrLn "CreateTable"
    doCreateTableIfNotExists db

    putStrLn "PutItem"
    doPutItem db 1234

    putStrLn "UpdateItem"
    doUpdateItem db

    putStrLn "GetItem"
    counter <- doGetItem db
    print counter

    putStrLn "Done"
