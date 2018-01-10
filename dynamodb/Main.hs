{-|
Module      : Main
Description : DynamoDB demo
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Endpoint(..)
                    , awsConfig
                    , connect
                    , intToText
                    , parseInt
                    , withAWS
                    , wrapAWSService
                    )
import           AWSViaHaskell.AWSPrelude
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (void, when)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
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

wrapAWSService 'dynamoDB "DDBService" "DDBSession"

newtype TableName = TableName Text deriving Show

-- Creates a table in DynamoDB and waits until table is in active state
doCreateTableIfNotExists :: TableName -> DDBSession -> IO ()
doCreateTableIfNotExists (TableName tn) = withAWS $ do
    newlyCreated <- handling _ResourceInUseException (const (pure False)) $ do
        void $ send $ createTable
                        tn
                        (keySchemaElement "counter_name" Hash :| [])
                        (provisionedThroughput 5 5)
                        & ctAttributeDefinitions .~ [ attributeDefinition "counter_name" S ]
        return True
    when newlyCreated (void $ await tableExists (describeTable tn))

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: TableName -> DDBSession -> IO ()
doDeleteTableIfExists (TableName tn) = withAWS $ do
    deleted <- handling _ResourceNotFoundException (const (pure False)) $ do
        void $ send $ deleteTable tn
        return True
    when deleted (void $ await tableNotExists (describeTable tn))

-- Puts an item into the DynamoDB table
doPutItem :: TableName -> Int -> DDBSession -> IO ()
doPutItem (TableName tn) value = withAWS $ do
    void $ send $ putItem tn
                    & piItem .~ item
    where item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            , ("counter_value", attributeValue & avN .~ Just (intToText value))
            ]

-- Updates an item in the DynamoDB table
doUpdateItem :: TableName -> DDBSession -> IO ()
doUpdateItem (TableName tn) = withAWS $ do
    void $ send $ updateItem tn
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
doGetItem :: TableName -> DDBSession -> IO (Maybe Int)
doGetItem (TableName tn) = withAWS $ do
    result <- send $ getItem tn
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
    let tableName = TableName "table"

    ddbSession <- connect
                    (awsConfig (Local "localhost" 8000))
                    dynamoDBService

    putStrLn "DeleteTableIfExists"
    doDeleteTableIfExists tableName ddbSession

    putStrLn "CreateTableIfNotExists"
    doCreateTableIfNotExists tableName ddbSession

    putStrLn "PutItem"
    doPutItem tableName 1234 ddbSession

    putStrLn "UpdateItem"
    doUpdateItem tableName ddbSession

    putStrLn "GetItem"
    counter <- doGetItem tableName ddbSession
    print counter

    putStrLn "Done"
