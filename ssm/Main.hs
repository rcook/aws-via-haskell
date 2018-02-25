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
import           SSMImports

wrapAWSService 'ssm "SSMService" "SSMSession"

main :: IO ()
main = do
    ssmSession <- connect (awsConfig (AWSRegion Ohio)) ssmService

    putStrLn "Done"
