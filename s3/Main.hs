--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Monad (void)
import           Network.AWS
                    ( Region(..)
                    , send
                    )
import           Network.AWS.S3
                    ( BucketName(..)
                    , createBucket
                    , listBuckets
                    , s3
                    )

doListBuckets :: AWSInfo -> IO ()
doListBuckets = withAWS' $ do
    void $ send $ listBuckets

doCreateBucket :: AWSInfo -> IO ()
doCreateBucket = withAWS' $ do
    void $ send $ createBucket (BucketName "test-bucket")

main :: IO ()
main = do
    aws <- getAWSInfo LoggingEnabled Ohio s3

    putStrLn "ListBuckets"
    doListBuckets aws

    putStrLn "CreateBucket"
    doCreateBucket aws

    putStrLn "Hello from S3Demo.main"
