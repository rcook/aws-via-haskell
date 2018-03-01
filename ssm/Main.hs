{-|
Module      : Main
Description : SSM (Simple Systems Manager) demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import           SSMImports
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

wrapAWSService 'ssm "SSMService" "SSMSession"

newtype ParameterName = ParameterName Text

newtype ParameterValue = ParameterValue Text

doGetParameter :: ParameterName -> SSMSession -> IO (Text, Integer)
doGetParameter (ParameterName pn) = withAWS $ do
    result <- send $ getParameters (NonEmpty.fromList [pn])
    let param = head $ result ^. grsParameters
    return $ (fromJust (param ^. pValue), fromJust (param ^. pVersion))

doPutParameter :: ParameterName -> ParameterValue -> SSMSession -> IO ()
doPutParameter (ParameterName pn) (ParameterValue pv) = withAWS $
    void (send $ putParameter pn pv String & ppOverwrite .~ Just True)

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let conf = awsConfig (AWSRegion Ohio)
                & awscCredentials .~ (FromFile "aws-via-haskell" $ homeDir </> ".aws" </> "credentials")

    ssmSession <- connect conf ssmService

    putStrLn "doPutParameter"
    doPutParameter (ParameterName "/AAA/BBB") (ParameterValue "CCC") ssmSession

    putStrLn "doGetParameter"
    (value, version) <- doGetParameter (ParameterName "/AAA/BBB") ssmSession
    Text.putStrLn $ "Value: " <> value
    putStrLn $ "Version: " ++ show version

    putStrLn "Done"
