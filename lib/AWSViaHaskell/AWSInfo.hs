--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AWSViaHaskell.AWSInfo
    ( AWSAction
    , AWSConfig(..)
    , AWSConfig'(..)
    , AWSConnection(..)
    , LoggingState(..)
    , ServiceClass(..)
    , ServiceEndpoint(..)
    , SessionClass(..)
    , awsConfig
    , connect
    , getAWSConnection
    , withAWS
    , withAWS'
    , withAWSTyped
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

type AWSAction a = AWSConnection -> IO a

type HostName = ByteString

type Port = Int

data AWSConfig = AWSConfig
    { acServiceEndpoint :: ServiceEndpoint
    , acService :: Service
    , acLoggingState :: LoggingState
    , acCredentials :: Credentials
    }

data AWSConnection = AWSConnection
    { acxEnv :: Env
    , acxRegion :: Region
    , acxService :: Service
    }

data LoggingState = LoggingEnabled | LoggingDisabled

data ServiceEndpoint = AWS Region | Local HostName Port

awsConfig :: ServiceEndpoint -> Service -> AWSConfig
awsConfig serviceEndpoint service = AWSConfig serviceEndpoint service LoggingDisabled Discover

getAWSConnection :: AWSConfig -> IO AWSConnection
getAWSConnection AWSConfig{..} = do
    e <- mkEnv acLoggingState acCredentials
    let (r, s) = regionService acServiceEndpoint acService
    return $ AWSConnection e r s
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
    -> AWSConnection
    -> m a
withAWS action AWSConnection{..} =
    runResourceT . runAWST acxEnv . within acxRegion $ do
        reconfigure acxService action

withAWS' :: MonadBaseControl IO m =>
    AWSConnection
    -> AWST' Env (ResourceT m) a
    -> m a
withAWS' = flip withAWS

class ServiceClass a where
    type TypedSession a :: *
    rawService :: a -> Service
    wrappedSession :: AWSConnection -> TypedSession a

class SessionClass a where
    rawSession :: a -> AWSConnection

data AWSConfig' = AWSConfig'
    { acServiceEndpoint' :: ServiceEndpoint
    , acLoggingState' :: LoggingState
    , acCredentials' :: Credentials
    }

connect :: forall a . ServiceClass a => AWSConfig' -> a -> IO (TypedSession a)
connect AWSConfig'{..} service = do
    let serviceRaw = rawService service
    --session' <- getAWSConnection undefined
    let awsConfig' = AWSConfig acServiceEndpoint' serviceRaw acLoggingState' acCredentials'
    session' <- getAWSConnection awsConfig'
    let session = wrappedSession @a session'
    return session

withAWSTyped :: (MonadBaseControl IO m, SessionClass b) =>
    AWST' Env (ResourceT m) a
    -> b
    -> m a
withAWSTyped action session =
    let sessionRaw = rawSession session
    in withAWS action sessionRaw
