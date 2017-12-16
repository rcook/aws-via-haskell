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
doCreateBucket aws = withAWS aws $ do
    void $ send $ createBucket (BucketName "test-bucket")

main :: IO ()
main = do
    aws <- getAWSInfo LoggingEnabled Ohio s3
    doCreateBucket aws
    putStrLn "Hello from S3Demo.main"
