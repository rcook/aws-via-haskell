--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( AWSConfig'(..)
                    , AWSConnection(..)
                    , LoggingState(..)
                    , ServiceClass(..)
                    , ServiceEndpoint(..)
                    , SessionClass(..)
                    , connect
                    , withAWSTyped
                    )
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (forM_, void, when)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Conduit.Binary (sinkLbs)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS
                    ( Credentials(..)
                    , Region(..)
                    , Service
                    , await
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

data S3Service = S3Service Service

instance ServiceClass S3Service where
    type TypedSession S3Service = S3Session
    rawService (S3Service raw) = raw
    wrappedSession = S3Session

data S3Session = S3Session AWSConnection

instance SessionClass S3Session where
    rawSession (S3Session raw) = raw

s3Service :: S3Service
s3Service = S3Service s3

doCreateBucketIfNotExists :: BucketName -> S3Session -> IO ()
doCreateBucketIfNotExists bucketName s3Session@(S3Session session) = (flip withAWSTyped) s3Session $ do
    let cbc = createBucketConfiguration
                & cbcLocationConstraint .~ Just (LocationConstraint (acxRegion session))
    newlyCreated <- handling _BucketAlreadyOwnedByYou (const (pure False)) $ do
        void $ send $ createBucket bucketName
                        & cbCreateBucketConfiguration .~ Just cbc
        return True
    when newlyCreated (void $ await bucketExists (headBucket bucketName))

doListBuckets :: S3Session -> IO [BucketName]
doListBuckets = withAWSTyped $ do
    result <- send $ listBuckets
    return $ [ x ^. bName | x <- result ^. lbrsBuckets ]

doPutObject :: BucketName -> S3Session -> IO ()
doPutObject bucketName = withAWSTyped $ do
    void $ send $ putObject bucketName "object-key" "object-bytes"

doListObjects :: BucketName -> S3Session -> IO [ObjectKey]
doListObjects bucketName = withAWSTyped $ do
    result <- send $ listObjectsV bucketName
    return $ [ x ^. oKey | x <- result ^. lrsContents ]

doGetObject :: BucketName -> S3Session -> IO ByteString
doGetObject bucketName = withAWSTyped $ do
    result <- send $ getObject bucketName "object-key"
    (result ^. gorsBody) `sinkBody` sinkLbs

main :: IO ()
main = do
    let bucketName = "rcook456dac3a5a0e4aeba1b3238306916a31"

    s3Session <- connect (AWSConfig' (AWS Ohio) LoggingDisabled Discover) s3Service
    --s3Session <- connect (AWSConfig' (Local "localhost" 4572) LoggingDisabled Discover) s3Service

    putStrLn "CreateBucket"
    doCreateBucketIfNotExists bucketName s3Session

    putStrLn "ListBuckets"
    bucketNames <- doListBuckets s3Session
    forM_ bucketNames $ \n ->
        Text.putStrLn $ "  " <> toText n

    putStrLn "PutObject"
    doPutObject bucketName s3Session

    putStrLn "ListObjects"
    objectKeys <- doListObjects bucketName s3Session
    forM_ objectKeys $ \k ->
        Text.putStrLn $ "  " <> toText k

    putStrLn "GetObject"
    content <- doGetObject bucketName s3Session
    ByteString.putStrLn $ "  " <> content
