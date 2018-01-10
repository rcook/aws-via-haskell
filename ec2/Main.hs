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
import           AWSViaHaskell.Prelude
import           Control.Exception.Lens (handling)
import           Control.Monad (forM_, void)
import           Control.Lens ((^.), (&), (.~), Getting)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (readFile)
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Monoid ((<>), First)
import           Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS.EC2
                    ( createSecurityGroup
                    , describeSecurityGroups
                    , dsgrsSecurityGroups
                    , ec2
                    , runInstances
                    , sgGroupName
                    )
import           Network.AWS.EC2
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

wrapAWSService 'ec2 "EC2Service" "EC2Session"

newtype GroupName = GroupName Text
newtype ImageDescription = ImageDescription (Maybe Text)
newtype ImageId = ImageId Text
newtype KeyMaterial = KeyMaterial ByteString
newtype KeyName = KeyName Text

-- Amazon Linux AMI 2017.09.1 (HVM), SSD Volume Type
freeImageId :: ImageId
freeImageId = ImageId "ami-caaf84af"

keyName :: KeyName
keyName = KeyName "My key"

doImportKeyPairIfNotExists :: KeyName -> KeyMaterial -> EC2Session -> IO ()
doImportKeyPairIfNotExists (KeyName kn) (KeyMaterial km) = withAWS $
    handling _DuplicateKeyPair (const $ pure ()) $
        void $ send $ importKeyPair kn km
    where
        -- [NOTE] Demonstrates how to define a custom error matcher
        _DuplicateKeyPair :: AsError a => Getting (First ServiceError) a ServiceError
        _DuplicateKeyPair = _ServiceError . hasStatus 400 . hasCode "InvalidKeyPair.Duplicate"

doDescribeKeyPairs :: EC2Session -> IO [KeyName]
doDescribeKeyPairs = withAWS $ do
    result <- send describeKeyPairs
    return $ catMaybes [ KeyName <$> (keyPair ^. kpiKeyName) | keyPair <- result ^. dkprsKeyPairs ]

doCreateSecurityGroupIfNotExists :: EC2Session -> IO ()
doCreateSecurityGroupIfNotExists = withAWS $
    handling _DuplicateGroup (const $ pure ()) $
        void $ send $ createSecurityGroup "My security group" "MySecurityGroup"
    where
        -- [NOTE] Demonstrates how to define a custom error matcher
        _DuplicateGroup :: AsError a => Getting (First ServiceError) a ServiceError
        _DuplicateGroup = _ServiceError . hasStatus 400 . hasCode "InvalidGroup.Duplicate"

doDescribeSecurityGroups :: EC2Session -> IO [GroupName]
doDescribeSecurityGroups = withAWS $ do
    result <- send $ describeSecurityGroups
    return [GroupName $ sg ^. sgGroupName | sg <- result ^. dsgrsSecurityGroups ]

doDescribeImages :: [ImageId] -> EC2Session -> IO [(ImageId, ImageDescription)]
doDescribeImages imageIds = withAWS $ do
    result <- send $ describeImages
                        & deseImageIds .~ map (\(ImageId iid) -> iid) imageIds
    return $ [ (ImageId (i ^. iImageId), ImageDescription (i ^. iDescription)) | i <- result ^. desrsImages ]

doRunInstances :: ImageId -> KeyName -> EC2Session -> IO ()
doRunInstances (ImageId iid) (KeyName kn) = withAWS $
    void $ send $ runInstances iid 1 1
                    & rKeyName .~ Just kn
                    -- & rSecurityGroups ["default"]

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let publicKeyPath = homeDir </> ".ssh" </> "id_rsa.pub"
    publicKeyMaterial <- KeyMaterial <$> ByteString.readFile publicKeyPath

    ec2Session <- connect
                    (awsConfig (AWSRegion Ohio))
                    ec2Service

    putStrLn "ImportKeyPair"
    doImportKeyPairIfNotExists keyName publicKeyMaterial ec2Session

    putStrLn "DescribeKeyPairs"
    keyNames <- doDescribeKeyPairs ec2Session
    forM_ keyNames $ \(KeyName kn) -> Text.putStrLn $ "  " <> kn

    {-
    putStrLn "CreateSecurityGroup"
    doCreateSecurityGroup ec2Session

    putStrLn "DescribeSecurityGroups"
    groupNames <- doDescribeSecurityGroups ec2Session
    forM_ groupNames $ \(GroupName groupName) ->
        Text.putStrLn $ "  " <> groupName
    -}

    --putStrLn "DescribeImages"
    --infos <- doDescribeImages [freeImageId] ec2Session
    --forM_ infos $ \(ImageId imageId, ImageDescription description) ->
    --    Text.putStrLn $ "  " <> imageId <> ": " <> fromMaybe "(no description)" description

    --putStrLn "RunInstances"
    --doRunInstances freeImageId keyName ec2Session

    putStrLn "Done"
