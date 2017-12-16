--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Lens ((^.), (.~), (&))
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
                    , LocationConstraint(..)
                    , cbCreateBucketConfiguration
                    , cbcLocationConstraint
                    , createBucketConfiguration
                    , bName
                    , createBucket
                    , lbrsBuckets
                    , listBuckets
                    , s3
                    )

data S3Info = S3Info
    { aws :: AWSInfo
    , bucketName :: BucketName
    }

getS3Info :: LoggingState -> Region -> IO S3Info
getS3Info loggingState region = do
    aws <- getAWSInfo loggingState region s3
    return $ S3Info aws "rcook456dac3a5a0e4aeba1b3238306916a31"

doListBuckets :: S3Info -> IO [BucketName]
doListBuckets S3Info{..} = withAWS' aws $ do
    result <- send $ listBuckets
    return $ [ x ^. bName | x <- result ^. lbrsBuckets ]

doCreateBucket :: S3Info -> IO ()
doCreateBucket S3Info{..} = withAWS' aws $ do
    let cbc = createBucketConfiguration
                & cbcLocationConstraint .~ Just (LocationConstraint (region aws))
    void $ send $ createBucket bucketName
                    & cbCreateBucketConfiguration .~ Just cbc

main :: IO ()
main = do
    s3Info <- getS3Info LoggingEnabled Ohio

    putStrLn "CreateBucket"
    doCreateBucket s3Info

    putStrLn "ListBuckets"
    bucketNames <- doListBuckets s3Info
    forM_ bucketNames $
        \n -> Text.putStrLn $ "  " <> toText n
