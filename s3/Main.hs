--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (forM_, void, when)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS
                    ( Region(..)
                    , await
                    , send
                    )
import           Network.AWS.Data (toText)
import           Network.AWS.S3
                    ( _BucketAlreadyOwnedByYou
                    , BucketName(..)
                    , LocationConstraint(..)
                    , bucketExists
                    , cbCreateBucketConfiguration
                    , cbcLocationConstraint
                    , createBucketConfiguration
                    , bName
                    , createBucket
                    , headBucket
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

doCreateBucketIfNotExists :: S3Info -> IO ()
doCreateBucketIfNotExists S3Info{..} = withAWS' aws $ do
    let cbc = createBucketConfiguration
                & cbcLocationConstraint .~ Just (LocationConstraint (region aws))
    newlyCreated <- handling _BucketAlreadyOwnedByYou (const (pure False)) $ do
        void $ send $ createBucket bucketName
                        & cbCreateBucketConfiguration .~ Just cbc
        return True
    when newlyCreated (void $ await bucketExists (headBucket bucketName))

main :: IO ()
main = do
    s3Info <- getS3Info LoggingDisabled Ohio

    putStrLn "CreateBucket"
    doCreateBucketIfNotExists s3Info

    putStrLn "ListBuckets"
    bucketNames <- doListBuckets s3Info
    forM_ bucketNames $
        \n -> Text.putStrLn $ "  " <> toText n
