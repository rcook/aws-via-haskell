--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Monad (void)
import           Control.Monad.Trans.AWS
                    ( Region(..)
                    , send
                    )
import           Network.AWS.S3 (BucketName(..), createBucket, s3)

doCreateBucket :: AWSInfo -> IO ()
doCreateBucket AWSInfo{..} = withAWS env service region $ do
    void $ send $ createBucket (BucketName "test-bucket")

main :: IO ()
main = do
    awsInfo <- getAWSInfo LoggingEnabled Ohio s3
    doCreateBucket awsInfo
    putStrLn "Hello from S3Demo.main"
