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

module AWSViaHaskell.AWS
    ( Config(..)
    , LoggingState(..)
    , ServiceClass(..)
    , ServiceEndpoint(..)
    , Session(..)
    , SessionClass(..)
    , connect
    , withAWS
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

data Session = Session
    { acxEnv :: Env
    , acxRegion :: Region
    , acxService :: Service
    }

data LoggingState = LoggingEnabled | LoggingDisabled

data ServiceEndpoint = AWS Region | Local HostName Port

class ServiceClass a where
    type TypedSession a :: *
    rawService :: a -> Service
    wrappedSession :: Session -> TypedSession a

class SessionClass a where
    rawSession :: a -> Session

data Config = Config
    { acServiceEndpoint :: ServiceEndpoint
    , acLoggingState :: LoggingState
    , acCredentials :: Credentials
    }

connect :: forall a . ServiceClass a => Config -> a -> IO (TypedSession a)
connect Config{..} service = do
    let serviceRaw = rawService service
    session' <- getSession acServiceEndpoint serviceRaw acLoggingState acCredentials
    let session = wrappedSession @a session'
    return session

getSession :: ServiceEndpoint -> Service -> LoggingState -> Credentials -> IO Session
getSession acServiceEndpoint acService acLoggingState acCredentials = do
    e <- mkEnv acLoggingState acCredentials
    let (r, s) = regionService acServiceEndpoint acService
    return $ Session e r s
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

withAWS :: (MonadBaseControl IO m, SessionClass b) =>
    AWST' Env (ResourceT m) a
    -> b
    -> m a
withAWS action session =
    let Session{..} = rawSession session
    in
        runResourceT . runAWST acxEnv . within acxRegion $ do
            reconfigure acxService action
