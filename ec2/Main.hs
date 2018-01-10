{-|
Module      : Main
Description : EC2 demo
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
                    ( Endpoint(..)
                    , awsConfig
                    , connect
                    , withAWS
                    , wrapAWSService
                    )
import           Control.Exception.Lens (handling)
import           Control.Monad (forM_, void)
import           Control.Lens ((^.), Getting)
import           Data.Monoid ((<>), First)
import           Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS
                    ( _ServiceError
                    , AsError
                    , Region(..)
                    , ServiceError
                    , send
                    )
import           Network.AWS.EC2
                    ( createSecurityGroup
                    , describeSecurityGroups
                    , dsgrsSecurityGroups
                    , ec2
                    , runInstances
                    , sgGroupName
                    )
import           Network.AWS.Error (hasCode, hasStatus)

wrapAWSService 'ec2 "EC2Service" "EC2Session"

newtype GroupName = GroupName Text
newtype ImageId = ImageId Text deriving Show

doCreateSecurityGroup :: EC2Session -> IO ()
doCreateSecurityGroup = withAWS $
    handling _DuplicateGroup (const $ pure ()) $
        void $ send $ createSecurityGroup "My security group" "MySecurityGroup"
    where
        _DuplicateGroup :: AsError a => Getting (First ServiceError) a ServiceError
        _DuplicateGroup = _ServiceError . hasStatus 400 . hasCode "InvalidGroup.Duplicate"

doDescribeSecurityGroups :: EC2Session -> IO [GroupName]
doDescribeSecurityGroups = withAWS $ do
    result <- send $ describeSecurityGroups
    return [GroupName $ sg ^. sgGroupName | sg <- result ^. dsgrsSecurityGroups ]

doRunInstances :: ImageId -> EC2Session -> IO ()
doRunInstances (ImageId iid) = withAWS $
    void $ send $ runInstances iid 1 1

main :: IO ()
main = do
    ec2Session <- connect
                    (awsConfig (AWSRegion Ohio))
                    ec2Service

    putStrLn "CreateSecurityGroup"
    doCreateSecurityGroup ec2Session

    putStrLn "DescribeSecurityGroups"
    groupNames <- doDescribeSecurityGroups ec2Session
    forM_ groupNames $ \(GroupName groupName) ->
        Text.putStrLn $ "  " <> groupName

    putStrLn "RunInstances"
    let imageId = ImageId "foobar"
    doRunInstances imageId ec2Session

    putStrLn "Done"
