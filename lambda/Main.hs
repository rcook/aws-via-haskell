--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Config(..)
                    , LoggingState(..)
                    , ServiceClass(..)
                    , ServiceEndpoint(..)
                    , Session
                    , SessionClass(..)
                    , connect
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
import           Data.Aeson (Value(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
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
                    ( Credentials(..)
                    , Region(..)
                    , Service
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
                    , invoke
                    , irsPayload
                    , lambda
                    , listFunctions
                    , lfrsFunctions
                    )
import           Network.AWS.STS
                    ( gcirsAccount
                    , getCallerIdentity
                    , sts
                    )
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

data STSService = STSService Service

instance ServiceClass STSService where
    type TypedSession STSService = STSSession
    rawService (STSService raw) = raw
    wrappedSession = STSSession

data STSSession = STSSession Session

instance SessionClass STSSession where
    rawSession (STSSession raw) = raw

data LambdaService = LambdaService Service

instance ServiceClass LambdaService where
    type TypedSession LambdaService = LambdaSession
    rawService (LambdaService raw) = raw
    wrappedSession = LambdaSession

data LambdaSession = LambdaSession Session

instance SessionClass LambdaSession where
    rawSession (LambdaSession raw) = raw

stsService :: STSService
stsService = STSService sts

lambdaService :: LambdaService
lambdaService = LambdaService lambda

newtype AccountID = AccountID Text deriving Show

newtype FunctionName = FunctionName Text deriving Show

newtype Role = Role Text deriving Show

newtype Handler = Handler Text deriving Show

type Payload = HashMap Text Value

doGetAccountID :: STSSession -> IO (Maybe AccountID)
doGetAccountID = withAWS $ do
    result <- send getCallerIdentity
    return $ AccountID <$> result ^. gcirsAccount

-- Must have a role named "lambda_basic_execution" with AWSLambdaBasicExecutionRole policy attached
lambdaBasicExecutionRole :: AccountID -> Role
lambdaBasicExecutionRole (AccountID s) = Role $ Text.toStrict (format "arn:aws:iam::{}:role/lambda_basic_execution" $ Only s)

zipFunctionCode :: FilePath -> POSIXTime -> ByteString -> FunctionCode
zipFunctionCode path timestamp sourceCode =
    let entry = toEntry path (floor timestamp) sourceCode
        archive = entry `addEntryToArchive` emptyArchive
        bytes = ByteString.toStrict $ fromArchive archive
    in functionCode & fcZipFile .~ Just bytes

doListFunctions :: LambdaSession -> IO [Maybe FunctionName]
doListFunctions = withAWS $ do
    result <- send $ listFunctions
    return [ FunctionName <$> f ^. fcFunctionName | f <- result ^. lfrsFunctions ]

doDeleteFunctionIfExists :: FunctionName -> LambdaSession -> IO ()
doDeleteFunctionIfExists (FunctionName fn) = withAWS $ do
    handling (_ResourceNotFoundException) (const (pure ())) $ do
        void $ send $ deleteFunction fn

doCreateFunctionIfNotExists :: FunctionName -> Runtime -> Role -> Handler -> FunctionCode -> LambdaSession -> IO ()
doCreateFunctionIfNotExists (FunctionName fn) rt (Role r) (Handler h) fc = withAWS $ do
    handling _ResourceConflictException (const (pure ())) $ do
        void $ send $ createFunction fn rt r h fc

doInvoke :: FunctionName -> Payload -> LambdaSession -> IO (Maybe Payload)
doInvoke (FunctionName fn) payload = withAWS $ do
    result <- send $ invoke fn payload
    return $ result ^. irsPayload

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let config = Config
                    (AWS Ohio)
                    LoggingDisabled
                    (FromFile "aws-via-haskell" $ homeDir </> ".aws" </> "credentials")

    stsSession <- connect config stsService
    lambdaSession <- connect config lambdaService

    -- Get AWS account ID
    -- TODO: Deliberately blow up if we don't have one!
    Just accountID <- doGetAccountID stsSession
    let role = lambdaBasicExecutionRole accountID
    print role

    let fn = FunctionName "Add"

    putStrLn "DeleteFunction"
    doDeleteFunctionIfExists fn lambdaSession

    timestamp <- getPOSIXTime
    let fc = zipFunctionCode "add_handler.py" timestamp "def add_handler(event, context):\n\
        \    x = int(event[\"x\"])\n\
        \    y = int(event[\"y\"])\n\
        \    return { \"result\" : x + y }"

    putStrLn "CreateFunction"
    doCreateFunctionIfNotExists fn PYTHON2_7 role (Handler "add_handler.add_handler") fc lambdaSession

    putStrLn "ListFunctions"
    names <- doListFunctions lambdaSession
    forM_ names $ \mbName ->
        case mbName of
            Just name -> putStrLn $ "  " <> show name
            Nothing -> Text.putStrLn $ "  (unnamed)"

    putStrLn "Invoke"
    result <- doInvoke fn (HashMap.fromList [ ("x", Number 10), ("y", Number 25) ]) lambdaSession
    print result