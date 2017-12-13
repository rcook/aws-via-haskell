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
import           Control.Monad.Trans.AWS hiding (await)
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
                    )
import           System.IO (stdout)

type HostName = ByteString

type Port = Int

data LoggingState = LoggingEnabled | LoggingDisabled

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

-- Creates a table in DynamoDB and waits until table is in active state
-- Demonstrates:
-- * Use of runResourceT, runAWST
-- * Use of reconfigure
-- * How to handle exceptions in lenses
-- * Basic use of amazonka-style lenses
-- * How to wait on an asynchronous operation
doCreateTableIfNotExists :: DBInfo -> IO ()
doCreateTableIfNotExists DBInfo{..} = do
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
doDeleteTableIfExists :: DBInfo -> IO ()
doDeleteTableIfExists DBInfo{..} = do
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            exists <- handling _ResourceNotFoundException (const (pure False)) $ do
                void $ send $ deleteTable tableName
                return True
            when exists (void $ await tableNotExists (describeTable tableName))

-- Puts an item into the DynamoDB table
doPutItem :: DBInfo -> Int -> IO ()
doPutItem DBInfo{..} value = do
    let item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            , ("counter_value", attributeValue & avN .~ Just (intToText value))
            ]
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            void $ send $ putItem tableName & piItem .~ item

-- Gets an item from the DynamoDB table
doGetItem :: DBInfo -> IO (Maybe Int)
doGetItem DBInfo{..} = do
    let key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            ]
    runResourceT . runAWST env . within region $ do
        reconfigure service $ do
            result <- send $ getItem tableName & giKey .~ key
            return $ do
                valueAttr <- HashMap.lookup "counter_value" (result ^. girsItem)
                valueNStr <- valueAttr ^. avN
                parseInt valueNStr

main :: IO ()
main = do
    --env <- getDBInfo LoggingEnabled (AWS Ohio)
    env <- getDBInfo LoggingDisabled (Local "localhost" 8000)

    putStrLn "DeleteTable"
    doDeleteTableIfExists env

    putStrLn "CreateTable"
    doCreateTableIfNotExists env

    putStrLn "PutItem"
    doPutItem env 1234

    putStrLn "GetItem"
    counter <- doGetItem env
    print counter

    putStrLn "Done"
