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
import           Control.Lens ((^.))
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Network.AWS
                    ( send )
import           Network.AWS.Lambda
                    ( fcFunctionName
                    , lambda
                    , listFunctions
                    , lfrsFunctions
                    )

doListFunctions :: AWSInfo -> IO [Maybe Text]
doListFunctions = withAWS $ do
    result <- send $ listFunctions
    return [ f ^. fcFunctionName | f <- result ^. lfrsFunctions ]

main :: IO ()
main = do
    awsInfo <- getAWSInfo LoggingDisabled (Local "localhost" 4574) lambda

    putStrLn "ListFunctions"
    names <- doListFunctions awsInfo
    forM_ names $ \mbName ->
        case mbName of
            Just name -> Text.putStrLn $ "  " <> name
            Nothing -> Text.putStrLn $ "  (unnamed)"
