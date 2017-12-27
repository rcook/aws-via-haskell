--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Endpoint(..)
                    , ServiceClass(..)
                    , Session
                    , SessionClass(..)
                    , cCredentials
                    , config
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
import           Data.Text.Format (format)
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.IO as Text (putStrLn)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Network.AWS
                    ( Credentials(..)
                    , Region(..)
                    , Service
                    , send
                    )
import           Network.AWS.IAM
                    ( _EntityAlreadyExistsException
                    , createRole
                    , crrsRole
                    , iam
                    , rARN
                    )
import Network.AWS.IAM
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
import           System.Exit (exitSuccess)
import           System.FilePath ((</>))

-- TODO: Figure out how to reduce the class instance boilerplate!

data IAMService = IAMService Service

instance ServiceClass IAMService where
    type TypedSession IAMService = IAMSession
    rawService (IAMService raw) = raw
    wrappedSession = IAMSession

data IAMSession = IAMSession Session

instance SessionClass IAMSession where
    rawSession (IAMSession raw) = raw

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

iamService :: IAMService
iamService = IAMService iam

stsService :: STSService
stsService = STSService sts

lambdaService :: LambdaService
lambdaService = LambdaService lambda

newtype AccountID = AccountID Text deriving Show

newtype ARN = ARN Text deriving Show

newtype FunctionName = FunctionName Text deriving Show

newtype PolicyDocument = PolicyDocument Text deriving Show

newtype RoleName = RoleName Text deriving Show

newtype Handler = Handler Text deriving Show

type Payload = HashMap Text Value

doGetAccountID :: STSSession -> IO (Maybe AccountID)
doGetAccountID = withAWS $ do
    result <- send getCallerIdentity
    return $ AccountID <$> result ^. gcirsAccount

doCreateRoleIfNotExists :: AccountID -> RoleName -> PolicyDocument -> IAMSession -> IO ARN
doCreateRoleIfNotExists (AccountID aid) (RoleName rn) (PolicyDocument pd) = withAWS $ do
    handling _EntityAlreadyExistsException (const $ pure (arn aid rn)) $ do
        result <- send $ createRole rn pd
        return $ ARN (result ^. crrsRole . rARN)
    where
        arn aid' rn' = ARN (Text.toStrict (format "arn:aws:iam::{}:role/{}" $ (aid', rn')))
    
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

doCreateFunctionIfNotExists :: FunctionName -> Runtime -> ARN -> Handler -> FunctionCode -> LambdaSession -> IO ()
doCreateFunctionIfNotExists (FunctionName fn) rt (ARN arn) (Handler h) fc = withAWS $ do
    handling _ResourceConflictException (const (pure ())) $ do
        void $ send $ createFunction fn rt arn h fc

doInvoke :: FunctionName -> Payload -> LambdaSession -> IO (Maybe Payload)
doInvoke (FunctionName fn) payload = withAWS $ do
    result <- send $ invoke fn payload
    return $ result ^. irsPayload

doAttachRolePolicy :: IAMSession -> IO ()
doAttachRolePolicy = withAWS $ do
    void $ send $ attachRolePolicy "lambda_basic_execution" "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let conf = config (AWSRegion Ohio)
                & cCredentials .~ (FromFile "aws-via-haskell" $ homeDir </> ".aws" </> "credentials")

    stsSession <- connect conf stsService
    mbAccountID <- doGetAccountID stsSession
    let accountID = case mbAccountID of
                        Nothing -> error "No AWS account ID!"
                        Just x -> x
        roleName = RoleName "lambda_basic_execution"
        policyDoc = PolicyDocument "{\n\
                        \    \"Version\": \"2012-10-17\",\n\
                        \    \"Statement\": [{\n\
                        \         \"Effect\": \"Allow\",\n\
                        \         \"Principal\": { \"Service\" : \"lambda.amazonaws.com\" },\n\
                        \         \"Action\": \"sts:AssumeRole\"\n\
                        \    }]\n\
                        \}"

    iamSession <- connect conf iamService

    putStrLn "CreateRole"
    arn <- doCreateRoleIfNotExists accountID roleName policyDoc iamSession

    putStrLn "AttachRolePolicy"
    doAttachRolePolicy iamSession

    lambdaSession <- connect conf lambdaService
    let fn = FunctionName "Add"

    putStrLn "DeleteFunction"
    doDeleteFunctionIfExists fn lambdaSession

    timestamp <- getPOSIXTime
    let fc = zipFunctionCode "add_handler.py" timestamp "def add_handler(event, context):\n\
        \    x = int(event[\"x\"])\n\
        \    y = int(event[\"y\"])\n\
        \    return { \"result\" : x + y }"

    putStrLn "CreateFunction"
    doCreateFunctionIfNotExists fn PYTHON2_7 arn (Handler "add_handler.add_handler") fc lambdaSession

    putStrLn "ListFunctions"
    names <- doListFunctions lambdaSession
    forM_ names $ \mbName ->
        case mbName of
            Just name -> putStrLn $ "  " <> show name
            Nothing -> Text.putStrLn $ "  (unnamed)"

    putStrLn "Invoke"
    result <- doInvoke fn (HashMap.fromList [ ("x", Number 10), ("y", Number 25) ]) lambdaSession
    print result