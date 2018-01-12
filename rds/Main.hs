{-|
Module      : Main
Description : RDS demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
import           AWSViaHaskell.Prelude
import           RDSImports

wrapAWSService 'rds "RDSService" "RDSSession"

main :: IO ()
main = do
    rdsSession <- connect (awsConfig (AWSRegion Ohio)) rdsService
    putStrLn "Done"
