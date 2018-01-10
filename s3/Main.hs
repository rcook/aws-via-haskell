{-|
Module      : Main
Description : S3 demo
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Endpoint(..)
                    , awsConfig
                    , connect
                    , sRegion
                    , withAWS
                    , wrapAWSService
                    )
import           AWSViaHaskell.Prelude
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (forM_, void, when)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Conduit.Binary (sinkLbs)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (putStrLn)
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

wrapAWSService 's3 "S3Service" "S3Session"

doCreateBucketIfNotExists :: BucketName -> S3Session -> IO ()
doCreateBucketIfNotExists bucketName s3Session@(S3Session session) = (flip withAWS) s3Session $ do
    let cbc = createBucketConfiguration
                & cbcLocationConstraint .~ Just (LocationConstraint (session ^. sRegion))
    newlyCreated <- handling _BucketAlreadyOwnedByYou (const (pure False)) $ do
        void $ send $ createBucket bucketName
                        & cbCreateBucketConfiguration .~ Just cbc
        return True
    when newlyCreated (void $ await bucketExists (headBucket bucketName))

doListBuckets :: S3Session -> IO [BucketName]
doListBuckets = withAWS $ do
    result <- send $ listBuckets
    return $ [ x ^. bName | x <- result ^. lbrsBuckets ]

doPutObject :: BucketName -> S3Session -> IO ()
doPutObject bucketName = withAWS $ do
    void $ send $ putObject bucketName "object-key" "object-bytes"

doListObjects :: BucketName -> S3Session -> IO [ObjectKey]
doListObjects bucketName = withAWS $ do
    result <- send $ listObjectsV bucketName
    return $ [ x ^. oKey | x <- result ^. lrsContents ]

doGetObject :: BucketName -> S3Session -> IO ByteString
doGetObject bucketName = withAWS $ do
    result <- send $ getObject bucketName "object-key"
    (result ^. gorsBody) `sinkBody` sinkLbs

main :: IO ()
main = do
    let bucketName = "rcook456dac3a5a0e4aeba1b3238306916a31"

    s3Session <- connect
                    (awsConfig (AWSRegion Ohio))
                    --(awsConfig (Local "localhost" 4572) LoggingDisabled Discover)
                    s3Service

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
