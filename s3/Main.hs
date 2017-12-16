--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Lens ((<&>), set)
import           Control.Monad (void)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , Credentials(..)
                    , Env
                    , HasEnv
                    , LogLevel(..)
                    , Region(..)
                    , envLogger
                    , newEnv
                    , newLogger
                    , reconfigure
                    , runAWST
                    , runResourceT
                    , send
                    , within
                    )
import Control.Monad.Trans.Resource (MonadBaseControl, ResourceT)
import           Network.AWS (Service)
import           Network.AWS.S3 (BucketName(..), createBucket, s3)
import           System.IO (stdout)

-- This is DBInfo (more or less) from dynamodb-demo
data AWSInfo = AWSInfo
    { env :: Env
    , service :: Service
    , region :: Region
    }

-- This is LoggingState from dynamodb-demo
data LoggingState = LoggingEnabled | LoggingDisabled

-- This is getDBInfo (more of less) from dynamodb-demo
getAWSInfo :: LoggingState -> Region -> IO AWSInfo
getAWSInfo loggingState region = do
    env <- getEnv loggingState
    return $ AWSInfo env s3 region
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

-- This is withDynamoDB from from dynamodb-demo
withAWS :: (HasEnv r, MonadBaseControl IO m) =>
    r
    -> Service
    -> Region
    -> AWST' r (ResourceT m) a
    -> m a
withAWS env service region action =
    runResourceT . runAWST env . within region $ do
        reconfigure service action

doCreateBucket :: AWSInfo -> IO ()
doCreateBucket AWSInfo{..} = withAWS env service region $ do
    void $ send $ createBucket (BucketName "test-bucket")

main :: IO ()
main = do
    awsInfo <- getAWSInfo LoggingEnabled Ohio
    doCreateBucket awsInfo
    putStrLn "Hello from S3Demo.main"
