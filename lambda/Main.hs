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
import qualified Data.Text.IO as Text (putStrLn)
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Network.AWS
                    ( send
                    )
import           Network.AWS.Lambda
                    ( _ResourceConflictException
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

type AWSAction a = AWSInfo -> IO a

newtype FunctionName = FunctionName Text deriving Show

newtype Role = Role Text deriving Show

newtype Handler = Handler Text deriving Show

doListFunctions :: AWSAction [Maybe FunctionName]
doListFunctions = withAWS $ do
    result <- send $ listFunctions
    return [ FunctionName <$> f ^. fcFunctionName | f <- result ^. lfrsFunctions ]

doDeleteFunctionIfExists :: FunctionName -> AWSAction ()
doDeleteFunctionIfExists (FunctionName fn) = withAWS $ do
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

main :: IO ()
main = do
    awsInfo <- getAWSInfo LoggingDisabled (Local "localhost" 4574) lambda

    let fn = FunctionName "Add"

    putStrLn "DeleteFunction"
    doDeleteFunctionIfExists fn awsInfo

    timestamp <- getPOSIXTime
    let fc = zipFunctionCode "add_handler.py" timestamp "def add_handler(event, context):\n\
        \    x = int(event[\"x\"])\n\
        \    y = int(event[\"y\"])\n\
        \    return { \"result\" : x + y }"

    putStrLn "CreateFunction"
    -- TODO: Figure out how to create roles, security groups etc.
    doCreateFunctionIfNotExists fn PYTHON2_7 (Role "ARN") (Handler "add_handler") fc awsInfo

    putStrLn "ListFunctions"
    names <- doListFunctions awsInfo
    forM_ names $ \mbName ->
        case mbName of
            Just name -> putStrLn $ "  " <> show name
            Nothing -> Text.putStrLn $ "  (unnamed)"
