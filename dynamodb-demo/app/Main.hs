--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception.Lens (handling)
import           Control.Lens
import           Control.Monad (void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS hiding (await)
import           Data.Conduit hiding (await)
import qualified Data.Conduit.List       as CL
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Network.AWS (await)
import           Network.AWS.DynamoDB
import           Network.AWS.DynamoDB.GetItem (girsItem)

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn

defaultRegion :: Region
defaultRegion = Ohio

tableName :: Text
tableName = "BAR"

{-
getDefaultEnv :: IO Env
getDefaultEnv = do
    logger <- newLogger Debug stdout
    newEnv Discover <&> set envLogger logger
-}
getDefaultEnv :: IO Env
getDefaultEnv = newEnv Discover

-- Creates a table in DynamoDB and waits until table is in active state
doCreateTableIfNotExists :: IO ()
doCreateTableIfNotExists = do
    env <- getDefaultEnv
    let db = setEndpoint False "localhost" 8000 dynamoDB
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
doDeleteTableIfExists :: IO ()
doDeleteTableIfExists = do
    env <- getDefaultEnv
    let db = setEndpoint False "localhost" 8000 dynamoDB
    runResourceT . runAWST env . within defaultRegion $ do
        reconfigure db $ do
            exists <- handling _ResourceNotFoundException (const (pure False)) $ do
                void $ send $ deleteTable tableName
                return True
            when exists (void $ await tableNotExists (describeTable tableName))

-- Puts an item into the DynamoDB table
doPutItem :: IO ()
doPutItem = do
    let item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "foo")
            , ("counter_value", attributeValue & avN .~ Just "1001")
            ]
    env <- getDefaultEnv
    let db = setEndpoint False "localhost" 8000 dynamoDB
    runResourceT . runAWST env . within defaultRegion $ do
        reconfigure db $ do
            void $ send $ putItem tableName & piItem .~ item

-- Gets an item from the DynamoDB table
doGetItem :: IO ()
doGetItem = do
    let key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "foo")
            ]
    env <- getDefaultEnv
    let db = setEndpoint False "localhost" 8000 dynamoDB
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
    putStrLn "DeleteTable"
    doDeleteTableIfExists

    putStrLn "CreateTable"
    doCreateTableIfNotExists

    putStrLn "PutItem"
    doPutItem

    putStrLn "GetItem"
    doGetItem

    putStrLn "Done"
