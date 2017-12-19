--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (forM_, void, when)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Conduit.Binary (sinkLbs)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS
                    ( await
                    , send
                    , sinkBody
                    )
import           Network.AWS.Data (toText)
import           Network.AWS.S3
                    ( _BucketAlreadyOwnedByYou
                    , BucketName(..)
                    , LocationConstraint(..)
                    , ObjectKey(..)
                    , bucketExists
                    , cbCreateBucketConfiguration
                    , cbcLocationConstraint
                    , createBucketConfiguration
                    , bName
                    , createBucket
                    , getObject
                    , gorsBody
                    , headBucket
                    , lbrsBuckets
                    , listBuckets
                    , listObjectsV
                    , lrsContents
                    , oKey
                    , putObject
                    , s3
                    )

data S3Info = S3Info
    { aws :: AWSInfo
    , bucketName :: BucketName
    }

getS3Info :: LoggingState -> ServiceEndpoint -> IO S3Info
getS3Info loggingState serviceEndpoint = do
    aws <- getAWSInfo loggingState serviceEndpoint s3
    return $ S3Info aws "rcook456dac3a5a0e4aeba1b3238306916a31"

doCreateBucketIfNotExists :: S3Info -> IO ()
doCreateBucketIfNotExists S3Info{..} = withAWS' aws $ do
    let cbc = createBucketConfiguration
                & cbcLocationConstraint .~ Just (LocationConstraint (region aws))
    newlyCreated <- handling _BucketAlreadyOwnedByYou (const (pure False)) $ do
        void $ send $ createBucket bucketName
                        & cbCreateBucketConfiguration .~ Just cbc
        return True
    when newlyCreated (void $ await bucketExists (headBucket bucketName))

doListBuckets :: S3Info -> IO [BucketName]
doListBuckets S3Info{..} = withAWS' aws $ do
    result <- send $ listBuckets
    return $ [ x ^. bName | x <- result ^. lbrsBuckets ]

doPutObject :: S3Info -> IO ()
doPutObject S3Info{..} = withAWS' aws $ do
    void $ send $ putObject bucketName "object-key" "object-bytes"
    return ()

doListObjects :: S3Info -> IO [ObjectKey]
doListObjects S3Info{..} = withAWS' aws $ do
    result <- send $ listObjectsV bucketName
    return $ [ x ^. oKey | x <- result ^. lrsContents ]

doGetObject :: S3Info -> IO ByteString
doGetObject S3Info{..} = withAWS' aws $ do
    result <- send $ getObject bucketName "object-key"
    (result ^. gorsBody) `sinkBody` sinkLbs

main :: IO ()
main = do
    -- localstack by default exposes its S3 service on port 4572
    s3Info <- getS3Info LoggingDisabled (Local "localhost" 4572)

    putStrLn "CreateBucket"
    doCreateBucketIfNotExists s3Info

    putStrLn "ListBuckets"
    bucketNames <- doListBuckets s3Info
    forM_ bucketNames $ \n ->
        Text.putStrLn $ "  " <> toText n

    putStrLn "PutObject"
    doPutObject s3Info

    putStrLn "ListObjects"
    objectKeys <- doListObjects s3Info
    forM_ objectKeys $ \k ->
        Text.putStrLn $ "  " <> toText k

    putStrLn "GetObject"
    content <- doGetObject s3Info
    ByteString.putStrLn content
