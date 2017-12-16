--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Lens ((^.))
import           Control.Monad (forM_, void)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS
                    ( Region(..)
                    , send
                    )
import           Network.AWS.Data (toText)
import           Network.AWS.S3
                    ( BucketName(..)
                    , bName
                    , createBucket
                    , lbrsBuckets
                    , listBuckets
                    , s3
                    )

doListBuckets :: AWSInfo -> IO [BucketName]
doListBuckets = withAWS $ do
    result <- send $ listBuckets
    return $ [ x ^. bName | x <- result ^. lbrsBuckets ]

doCreateBucket :: AWSInfo -> IO ()
doCreateBucket = withAWS $ do
    void $ send $ createBucket (BucketName "test-bucket")

main :: IO ()
main = do
    aws <- getAWSInfo LoggingDisabled Ohio s3

    putStrLn "ListBuckets"
    bucketNames <- doListBuckets aws
    forM_ bucketNames $
        \n -> Text.putStrLn $ "  " <> toText n

    --putStrLn "CreateBucket"
    --doCreateBucket aws
