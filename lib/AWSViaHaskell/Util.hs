--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module AWSViaHaskell.Util
    ( AWSInfo(..)
    , LoggingState(..)
    , getAWSInfo
    , withAWS
    ) where

import           Control.Lens ((<&>), set)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , Credentials(..)
                    , Env
                    , HasEnv
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

-- This is DBInfo (more or less) from dynamodb-demo
data AWSInfo = AWSInfo
    { env :: Env
    , region :: Region
    , service :: Service
    }

-- This is LoggingState from dynamodb-demo
data LoggingState = LoggingEnabled | LoggingDisabled

-- This is getDBInfo (more of less) from dynamodb-demo
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

-- This is withDynamoDB from from dynamodb-demo
withAWS :: (HasEnv r, MonadBaseControl IO m) =>
    r
    -> Service
    -> Region
    -> AWST' r (ResourceT m) a
    -> m a
withAWS e s r action =
    runResourceT . runAWST e . within r $ do
        reconfigure s action
