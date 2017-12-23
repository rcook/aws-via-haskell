--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AWSViaHaskell.AWSInfo
    ( AWSAction
    , AWSConfig(..)
    , AWSInfo(..)
    , LoggingState(..)
    , ServiceEndpoint(..)
    , awsConfig
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

type AWSAction a = AWSInfo -> IO a

type HostName = ByteString

type Port = Int

data AWSConfig = AWSConfig
    { acServiceEndpoint :: ServiceEndpoint
    , acService :: Service
    , acLoggingState :: LoggingState
    , acCredentials :: Credentials
    }

data AWSInfo = AWSInfo
    { env :: Env
    , region :: Region
    , service :: Service
    }

data LoggingState = LoggingEnabled | LoggingDisabled

data ServiceEndpoint = AWS Region | Local HostName Port

awsConfig :: ServiceEndpoint -> Service -> AWSConfig
awsConfig serviceEndpoint service = AWSConfig serviceEndpoint service LoggingDisabled Discover

getAWSInfo :: AWSConfig -> IO AWSInfo
getAWSInfo AWSConfig{..} = do
    e <- mkEnv acLoggingState acCredentials
    let (r, s) = regionService acServiceEndpoint acService
    return $ AWSInfo e r s
    where
        -- Standard discovery mechanism for credentials, log to standard output
        mkEnv LoggingEnabled c = do
            logger <- newLogger Debug stdout
            newEnv c <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        mkEnv LoggingDisabled c = newEnv c

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
