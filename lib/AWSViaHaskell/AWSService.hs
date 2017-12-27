--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AWSViaHaskell.AWSService
    ( Config
    , Endpoint(..)
    , Logging(..)
    , ServiceClass(..)
    , Session
    , SessionClass(..)
    , cCredentials
    , cEndpoint
    , cLogging
    , config
    , connect
    , sEnv
    , sRegion
    , sService
    , withAWS
    ) where

import           Control.Lens ((<&>), makeLenses, set)
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
    { _sEnv :: Env
    , _sRegion :: Region
    , _sService :: Service
    }
makeLenses ''Session

data Logging = LoggingEnabled | LoggingDisabled

data Endpoint = AWSRegion Region | Local HostName Port

data Config = Config
    { _cEndpoint :: Endpoint
    , _cLogging :: Logging
    , _cCredentials :: Credentials
    }
makeLenses ''Config

class ServiceClass a where
    type TypedSession a :: *
    rawService :: a -> Service
    wrappedSession :: Session -> TypedSession a

class SessionClass a where
    rawSession :: a -> Session

config :: Endpoint -> Config
config endpoint = Config endpoint LoggingDisabled Discover

connect :: forall a . ServiceClass a => Config -> a -> IO (TypedSession a)
connect Config{..} service = do
    let serviceRaw = rawService service
    e <- mkEnv _cLogging _cCredentials
    let (r, s) = regionService _cEndpoint serviceRaw
    session' <- return $ Session e r s
    let session = wrappedSession @a session'
    return session

mkEnv :: Logging -> Credentials -> IO Env
-- Standard discovery mechanism for credentials, log to standard output
mkEnv LoggingEnabled c = do
    logger <- newLogger Debug stdout
    newEnv c <&> set envLogger logger
-- Standard discovery mechanism for credentials, no logging
mkEnv LoggingDisabled c = newEnv c

regionService :: Endpoint -> Service -> (Region, Service)
-- Run against a DynamoDB instance running on AWS in specified region
regionService (AWSRegion region) s = (region, s)
-- Run against a local DynamoDB instance on a given host and port
regionService (Local hostName port) s = (NorthVirginia, setEndpoint False hostName port s)

withAWS :: (MonadBaseControl IO m, SessionClass b) =>
    AWST' Env (ResourceT m) a
    -> b
    -> m a
withAWS action session =
    let Session{..} = rawSession session
    in
        runResourceT . runAWST _sEnv . within _sRegion $ do
            reconfigure _sService action
