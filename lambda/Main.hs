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
                    , awscCredentials
                    , awsConfig
                    , connect
                    , withAWS
                    )
import           Codec.Archive.Zip
                    ( addEntryToArchive
                    , emptyArchive
                    , fromArchive
                    , toEntry
                    )
import           Control.Concurrent (threadDelay)
import           Control.Exception.Lens (handling)
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad (forM_, void)
import           Data.Aeson (Value(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import           Data.Maybe (catMaybes)
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
                    , _NoSuchEntityException
                    , apPolicyARN
                    , attachRolePolicy
                    , createRole
                    , crrsRole
                    , deleteRole
                    , detachRolePolicy
                    , iam
                    , larprsAttachedPolicies
                    , listAttachedRolePolicies
                    , rARN
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

newtype ARN = ARN Text deriving (Eq, Show)

newtype FunctionName = FunctionName Text deriving Show

newtype PolicyDocument = PolicyDocument Text deriving Show

newtype RoleName = RoleName Text deriving Show

newtype Handler = Handler Text deriving Show

type Payload = HashMap Text Value

awsLambdaBasicExecutionRolePolicy :: ARN
awsLambdaBasicExecutionRolePolicy = ARN "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"

doGetAccountID :: STSSession -> IO (Maybe AccountID)
doGetAccountID = withAWS $ do
    result <- send getCallerIdentity
    return $ AccountID <$> result ^. gcirsAccount

doDeleteFunctionIfExists :: FunctionName -> LambdaSession -> IO ()
doDeleteFunctionIfExists (FunctionName fn) = withAWS $ do
    handling (_ResourceNotFoundException) (const (pure ())) $ do
        void $ send $ deleteFunction fn

doDetachRolePolicyIfExists :: RoleName -> ARN -> IAMSession -> IO ()
doDetachRolePolicyIfExists (RoleName rn) (ARN arn) = withAWS $ do
    handling _NoSuchEntityException (const $ pure ()) $ do
        void $ send $ detachRolePolicy rn arn

doDeleteRoleIfExists :: RoleName -> IAMSession -> IO ()
doDeleteRoleIfExists (RoleName rn) = withAWS $ do
    handling _NoSuchEntityException (const $ pure ()) $ do
        void $ send $ deleteRole rn

doCreateRoleIfNotExists :: AccountID -> RoleName -> PolicyDocument -> IAMSession -> IO ARN
doCreateRoleIfNotExists (AccountID aid) (RoleName rn) (PolicyDocument pd) = withAWS $ do
    handling _EntityAlreadyExistsException (const $ pure (arn aid rn)) $ do
        result <- send $ createRole rn pd
        return $ ARN (result ^. crrsRole . rARN)
    where
        arn aid' rn' = ARN (Text.toStrict (format "arn:aws:iam::{}:role/{}" $ (aid', rn')))

doAttachRolePolicy :: RoleName -> ARN -> IAMSession -> IO ()
doAttachRolePolicy (RoleName rn) (ARN arn) = withAWS $ do
    void $ send $ attachRolePolicy rn arn

doListAttachedRolePolicies :: RoleName -> IAMSession -> IO [ARN]
doListAttachedRolePolicies (RoleName rn) = withAWS $ do
    result <- send $ listAttachedRolePolicies rn
    return $ catMaybes [ ARN <$> x ^. apPolicyARN | x <- result ^. larprsAttachedPolicies ]

waitForRolePolicy :: RoleName -> ARN -> IAMSession -> IO ()
waitForRolePolicy roleName policyArn iamSession = do
    arns <- doListAttachedRolePolicies roleName iamSession
    if policyArn `elem` arns then pure () else do
        threadDelay 1000000
        waitForRolePolicy roleName policyArn iamSession
        
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

doCreateFunctionIfNotExists :: FunctionName -> Runtime -> ARN -> Handler -> FunctionCode -> LambdaSession -> IO ()
doCreateFunctionIfNotExists (FunctionName fn) rt (ARN arn) (Handler h) fc = withAWS $ do
    handling _ResourceConflictException (const (pure ())) $ do
        void $ send $ createFunction fn rt arn h fc

doInvoke :: FunctionName -> Payload -> LambdaSession -> IO (Maybe Payload)
doInvoke (FunctionName fn) payload = withAWS $ do
    result <- send $ invoke fn payload
    return $ result ^. irsPayload

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let conf = awsConfig (AWSRegion Ohio)
                & awscCredentials .~ (FromFile "aws-via-haskell" $ homeDir </> ".aws" </> "credentials")

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

    lambdaSession <- connect conf lambdaService
    let fn = FunctionName "Add"

    putStrLn "DeleteFunctionIfExists"
    doDeleteFunctionIfExists fn lambdaSession

    iamSession <- connect conf iamService

    putStrLn "DetachRolePolicyIfExists"
    doDetachRolePolicyIfExists roleName awsLambdaBasicExecutionRolePolicy iamSession

    putStrLn "DeleteRoleIfExists"
    doDeleteRoleIfExists roleName iamSession

    putStrLn "CreateRole"
    arn <- doCreateRoleIfNotExists accountID roleName policyDoc iamSession

    putStrLn "AttachRolePolicy"
    doAttachRolePolicy roleName awsLambdaBasicExecutionRolePolicy iamSession

    putStrLn "WaitForRolePolicy"
    waitForRolePolicy roleName awsLambdaBasicExecutionRolePolicy iamSession

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