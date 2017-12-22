--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           AWSViaHaskell
                    ( AWSInfo(..)
                    , LoggingState(..)
                    , ServiceEndpoint(..)
                    , getAWSInfo
                    , withAWS
                    )
import           Codec.Archive.Zip
                    ( addEntryToArchive
                    , emptyArchive
                    , fromArchive
                    , toEntry
                    )
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (forM_, void)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Text.Format
                    ( Only(..)
                    , format
                    )
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.IO as Text (putStrLn)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Network.AWS
                    ( Region(..)
                    , send
                    )
import           Network.AWS.Lambda
                    ( _ResourceConflictException
                    , _ResourceNotFoundException
                    , FunctionCode
                    , Runtime(..)
                    , createFunction
                    , deleteFunction
                    , fcFunctionName
                    , fcZipFile
                    , functionCode
                    , lambda
                    , listFunctions
                    , lfrsFunctions
                    )
import           Network.AWS.STS
                    ( gcirsAccount
                    , getCallerIdentity
                    , sts
                    )

type AWSAction a = AWSInfo -> IO a

newtype AccountID = AccountID Text deriving Show

newtype FunctionName = FunctionName Text deriving Show

newtype Role = Role Text deriving Show

newtype Handler = Handler Text deriving Show

doGetAccountID :: AWSAction (Maybe AccountID)
doGetAccountID = withAWS $ do
    result <- send getCallerIdentity
    return $ AccountID <$> result ^. gcirsAccount

doListFunctions :: AWSAction [Maybe FunctionName]
doListFunctions = withAWS $ do
    result <- send $ listFunctions
    return [ FunctionName <$> f ^. fcFunctionName | f <- result ^. lfrsFunctions ]

doDeleteFunctionIfExists :: FunctionName -> AWSAction ()
doDeleteFunctionIfExists (FunctionName fn) = withAWS $ do
    handling (_ResourceNotFoundException) (const (pure ())) $ do
        void $ send $ deleteFunction fn

doCreateFunctionIfNotExists :: FunctionName -> Runtime -> Role -> Handler -> FunctionCode -> AWSAction ()
doCreateFunctionIfNotExists (FunctionName fn) rt (Role r) (Handler h) fc = withAWS $ do
    handling _ResourceConflictException (const (pure ())) $ do
        void $ send $ createFunction fn rt r h fc

zipFunctionCode :: FilePath -> POSIXTime -> ByteString -> FunctionCode
zipFunctionCode path timestamp sourceCode =
    let entry = toEntry path (floor timestamp) sourceCode
        archive = entry `addEntryToArchive` emptyArchive
        bytes = ByteString.toStrict $ fromArchive archive
    in functionCode & fcZipFile .~ Just bytes

lambdaBasicExecutionRole :: AccountID -> Role
lambdaBasicExecutionRole (AccountID s) = Role $ Text.toStrict (format "arn:aws:iam::{}:role/lambda_basic_execution" $ Only s)

main :: IO ()
main = do
    -- Use real AWS STS
    stsInfo <- getAWSInfo LoggingDisabled (AWS Ohio) sts
    -- Use localstack
    --stsInfo <- getAWSInfo LoggingDisabled (Local "localhost" 4574) sts

    -- Get AWS account ID
    -- TODO: Deliberately blow up if we don't have one!
    Just accountID <- doGetAccountID stsInfo
    let role = lambdaBasicExecutionRole accountID
    print role

    -- Use real AWS Lambda
    lambdaInfo <- getAWSInfo LoggingDisabled (AWS Ohio) lambda
    -- Use localstack
    --lambdaInfo <- getAWSInfo LoggingDisabled (Local "localhost" 4574) lambda

    let fn = FunctionName "Add"

    putStrLn "DeleteFunction"
    doDeleteFunctionIfExists fn lambdaInfo

    timestamp <- getPOSIXTime
    let fc = zipFunctionCode "add_handler.py" timestamp "def add_handler(event, context):\n\
        \    x = int(event[\"x\"])\n\
        \    y = int(event[\"y\"])\n\
        \    return { \"result\" : x + y }"

    putStrLn "CreateFunction"
    doCreateFunctionIfNotExists fn PYTHON2_7 role (Handler "add_handler") fc lambdaInfo

    putStrLn "ListFunctions"
    names <- doListFunctions lambdaInfo
    forM_ names $ \mbName ->
        case mbName of
            Just name -> putStrLn $ "  " <> show name
            Nothing -> Text.putStrLn $ "  (unnamed)"
