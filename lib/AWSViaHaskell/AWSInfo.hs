--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module AWSViaHaskell.AWSInfo
    ( AWSInfo(..)
    , LoggingState(..)
    , ServiceEndpoint(..)
    , getAWSInfo
    , withAWS
    , withAWS'
    ) where

import           Control.Lens ((<&>), set)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , reconfigure
                    , runAWST
                    , within
                    )
import           Control.Monad.Trans.Resource
                    ( MonadBaseControl
                    , ResourceT
                    )
import           Data.ByteString (ByteString)
import           Network.AWS
                    ( Credentials(..)
                    , Env
                    , LogLevel(..)
                    , Region(..)
                    , Service
                    , envLogger
                    , newEnv
                    , newLogger
                    , runResourceT
                    , setEndpoint
                    )
import           System.IO (stdout)

type HostName = ByteString

type Port = Int

data AWSInfo = AWSInfo
    { env :: Env
    , region :: Region
    , service :: Service
    }

data LoggingState = LoggingEnabled | LoggingDisabled

data ServiceEndpoint = AWS Region | Local HostName Port

getAWSInfo :: LoggingState -> ServiceEndpoint -> Service -> IO AWSInfo
getAWSInfo loggingState serviceEndpoint service = do
    e <- getEnv loggingState
    let (r, s) = regionService serviceEndpoint service
    return $ AWSInfo e r s
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

        -- Run against a DynamoDB instance running on AWS in specified region
        regionService (AWS region) s = (region, s)
        -- Run against a local DynamoDB instance on a given host and port
        regionService (Local hostName port) s = (NorthVirginia, setEndpoint False hostName port s)

withAWS :: MonadBaseControl IO m =>
    AWST' Env (ResourceT m) a
    -> AWSInfo
    -> m a
withAWS action AWSInfo{..} =
    runResourceT . runAWST env . within region $ do
        reconfigure service action

withAWS' :: MonadBaseControl IO m =>
    AWSInfo
    -> AWST' Env (ResourceT m) a
    -> m a
withAWS' = flip withAWS
