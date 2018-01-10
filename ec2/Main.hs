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
newtype InstanceId = InstanceId Text
newtype KeyMaterial = KeyMaterial ByteString
newtype KeyName = KeyName Text
newtype ReservationId = ReservationId Text

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

doDescribeImages :: [ImageId] -> EC2Session -> IO [(ImageId, ImageDescription)]
doDescribeImages imageIds = withAWS $ do
    result <- send $ describeImages
                        & deseImageIds .~ map (\(ImageId iid) -> iid) imageIds
    return $ [ (ImageId (i ^. iImageId), ImageDescription (i ^. iDescription)) | i <- result ^. desrsImages ]

doRunInstances :: ImageId -> KeyName -> EC2Session -> IO (ReservationId, InstanceId)
doRunInstances (ImageId iid) (KeyName kn) = withAWS $ do
    result <- send $ runInstances iid 1 1
                        & rKeyName .~ Just kn
                        & rInstanceType .~ Just T2_Micro
                        -- & rSecurityGroups ["default"]
    let reservationId = ReservationId (result ^. rReservationId)
        inst = head $ result ^. rInstances
        instId = InstanceId (inst ^. insInstanceId)
    return (reservationId, instId)

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

    putStrLn "DescribeImages"
    infos <- doDescribeImages [freeImageId] ec2Session
    forM_ infos $ \(ImageId imageId, ImageDescription description) ->
        Text.putStrLn $ "  " <> imageId <> ": " <> fromMaybe "(no description)" description

    putStrLn "RunInstances"
    (ReservationId rid, InstanceId instId) <- doRunInstances freeImageId keyName ec2Session
    Text.putStrLn $ "  Reservation ID: " <> rid
    Text.putStrLn $ "  Instance ID: " <> instId

    putStrLn "Done"
