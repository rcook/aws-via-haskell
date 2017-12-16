--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module AWSViaHaskell.Util
    ( AWSInfo
    , LoggingState(..)
    , getAWSInfo
    , withAWS
    ) where

import           Control.Lens ((<&>), set)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , Credentials(..)
                    , Env
                    , LogLevel(..)
                    , Region(..)
                    , Service
                    , envLogger
                    , newEnv
                    , newLogger
                    , reconfigure
                    , runAWST
                    , runResourceT
                    , within
                    )
import           Control.Monad.Trans.Resource
                    ( MonadBaseControl
                    , ResourceT
                    )
import           System.IO (stdout)

data AWSInfo = AWSInfo
    { env :: Env
    , region :: Region
    , service :: Service
    }

data LoggingState = LoggingEnabled | LoggingDisabled

getAWSInfo :: LoggingState -> Region -> Service -> IO AWSInfo
getAWSInfo loggingState r s = do
    e <- getEnv loggingState
    return $ AWSInfo e r s
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

withAWS :: MonadBaseControl IO m =>
    AWSInfo
    -> AWST' Env (ResourceT m) a
    -> m a
withAWS AWSInfo{..} action =
    runResourceT . runAWST env . within region $ do
        reconfigure service action
