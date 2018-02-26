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
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           SSMImports
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

wrapAWSService 'ssm "SSMService" "SSMSession"

newtype ParameterName = ParameterName Text

doGetParameter :: ParameterName -> SSMSession -> IO Text
doGetParameter (ParameterName pn) = withAWS $ do
    result <- send $ getParameters (NonEmpty.fromList [pn])
    return $ fromJust ((head $ result ^. grsParameters) ^. pValue)

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let conf = awsConfig (AWSRegion Ohio)
                & awscCredentials .~ (FromFile "aws-via-haskell" $ homeDir </> ".aws" </> "credentials")

    ssmSession <- connect conf ssmService

    value <- doGetParameter (ParameterName "/ViaHaskell/ExampleParameter") ssmSession
    print value

    putStrLn "Done"
