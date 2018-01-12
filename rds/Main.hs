{-|
Module      : Main
Description : RDS demo
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
import           Data.Text (Text)
import           RDSImports

wrapAWSService 'rds "RDSService" "RDSSession"

newtype InstanceId = InstanceId Text
newtype Password = Password Text
newtype AllocatedStorage = AllocatedStorage Int
newtype UserName = UserName Text

data DBInfo = DBInfo (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) deriving Show

doCreatePostgresInstance :: InstanceId -> UserName -> Password -> AllocatedStorage -> RDSSession -> IO (Maybe DBInfo)
doCreatePostgresInstance (InstanceId iid) (UserName u) (Password p) (AllocatedStorage a) = withAWS $ do
    result <- send $ createDBInstance iid "db.t2.micro" "postgres"
                        & cdiMasterUsername .~ Just u
                        & cdiMasterUserPassword .~ Just p
                        & cdiAllocatedStorage .~ Just a
    case result ^. cdirsDBInstance of
        Nothing -> return Nothing
        Just inst -> case inst ^. diEndpoint of
                        Nothing -> return $ Just $ DBInfo (inst ^. diEngineVersion) (inst ^. diDBInstanceARN) Nothing Nothing
                        Just endpoint -> return $ Just $ DBInfo
                                                            (inst ^. diEngineVersion)
                                                            (inst ^. diDBInstanceARN)
                                                            (endpoint ^. eAddress)
                                                            (endpoint ^. ePort)

main :: IO ()
main = do
    rdsSession <- connect (awsConfig (AWSRegion Ohio)) rdsService

    putStrLn "doCreatePostgresInstance"
    mbDBInfo <- doCreatePostgresInstance
                    (InstanceId "my-db-instance")
                    (UserName "username")
                    (Password "password")
                    (AllocatedStorage 5)
                    rdsSession
    case mbDBInfo of
        Nothing -> putStrLn "  (no DB info)"
        Just dbInfo -> print dbInfo

    putStrLn "Done"
