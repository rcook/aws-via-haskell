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
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (putStrLn)
import           Network.AWS.EC2
                    ( InstanceType(..)
                    , asgiGroupId
                    , asgiIPPermissions
                    , authorizeSecurityGroupIngress
                    , createSecurityGroup
                    , csgrsGroupId
                    , describeImages
                    , describeInstances
                    , describeInstanceStatus
                    , describeKeyPairs
                    , describeSecurityGroups
                    , deseImageIds
                    , desrsImages
                    , diiInstanceIds
                    , dirsReservations
                    , disInstanceIds
                    , dkprsKeyPairs
                    , dsgrsSecurityGroups
                    , dsgsGroupNames
                    , ec2
                    , iDescription
                    , iImageId
                    , importKeyPair
                    , insInstanceId
                    , insPublicDNSName
                    , instanceRunning
                    , instanceStatusOK
                    , ipFromPort
                    , ipIPRanges
                    , ipPermission
                    , ipRange
                    , ipToPort
                    , kpiKeyName
                    , rInstanceType
                    , rInstances
                    , rKeyName
                    , rReservationId
                    , rSecurityGroupIds
                    , runInstances
                    , sgGroupId
                    )
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

wrapAWSService 'ec2 "EC2Service" "EC2Session"

newtype DNSName = DNSName Text
newtype GroupId = GroupId Text
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

mySecurityGroupName :: GroupName
mySecurityGroupName = GroupName "my-security-group"

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

doCreateSecurityGroupIfNotExists :: GroupName -> EC2Session -> IO GroupId
doCreateSecurityGroupIfNotExists (GroupName gn) = withAWS $ do
    handling _DuplicateGroup (const getExisting) createNew
    where
        -- [NOTE] Demonstrates how to define a custom error matcher
        _DuplicateGroup :: AsError a => Getting (First ServiceError) a ServiceError
        _DuplicateGroup = _ServiceError . hasStatus 400 . hasCode "InvalidGroup.Duplicate"

        getExisting = do
            result <- send $ describeSecurityGroups
                                & dsgsGroupNames .~ [ gn ]
            return $ GroupId ((head $ result ^. dsgrsSecurityGroups) ^. sgGroupId)
        createNew = do
            csgResult <- send $ createSecurityGroup
                                    -- [NOTE] Curiously, these arguments are flipped in latest version of API (see https://github.com/brendanhay/amazonka/issues/439)
                                    gn
                                    "my-security-group-description"
            let gid = csgResult ^. csgrsGroupId
                inboundSSHPermission = ipPermission "tcp"
                                            & ipFromPort .~ Just 22
                                            & ipToPort .~ Just 22
                                            & ipIPRanges .~ [ ipRange "0.0.0.0/0" ]
            void $ send $ authorizeSecurityGroupIngress
                            & asgiGroupId .~ Just gid
                            & asgiIPPermissions .~ [ inboundSSHPermission ]
            return $ GroupId gid

doDescribeImages :: [ImageId] -> EC2Session -> IO [(ImageId, ImageDescription)]
doDescribeImages imageIds = withAWS $ do
    result <- send $ describeImages
                        & deseImageIds .~ map (\(ImageId iid) -> iid) imageIds
    return $ [ (ImageId (i ^. iImageId), ImageDescription (i ^. iDescription)) | i <- result ^. desrsImages ]

-- [NOTE] Run instance with default security group which should enable inbound ssh
doRunInstance :: ImageId -> KeyName -> GroupId -> EC2Session -> IO (ReservationId, InstanceId)
doRunInstance (ImageId iid) (KeyName kn) (GroupId gid) = withAWS $ do
    result <- send $ runInstances iid 1 1
                        & rKeyName .~ Just kn
                        & rInstanceType .~ Just T2_Micro
                        & rSecurityGroupIds .~ [ gid ]
    let reservationId = ReservationId (result ^. rReservationId)
        inst = head $ result ^. rInstances
        instId = InstanceId (inst ^. insInstanceId)
    return (reservationId, instId)

doWaitUntilInstanceRunning :: InstanceId -> EC2Session -> IO ()
doWaitUntilInstanceRunning (InstanceId iid) = withAWS $
    void $ await instanceRunning (describeInstances & diiInstanceIds .~ [ iid ])

doWaitUntilInstanceStatusOK :: InstanceId -> EC2Session -> IO ()
doWaitUntilInstanceStatusOK (InstanceId iid) = withAWS $
    void $ await instanceStatusOK (describeInstanceStatus & disInstanceIds .~ [ iid ])

doGetPublicDNSName :: InstanceId -> EC2Session -> IO (Maybe DNSName)
doGetPublicDNSName (InstanceId iid) = withAWS $ do
    result <- send $ describeInstances
                        & diiInstanceIds .~ [ iid ]
    let res = head $ result ^. dirsReservations
        inst = head $ res ^. rInstances
        mbDNSName = inst ^. insPublicDNSName
    return $ DNSName <$> mbDNSName

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let publicKeyPath = homeDir </> ".ssh" </> "id_rsa.pub"
        privateKeyPath = homeDir </> ".ssh" </> "id_rsa"

    ec2Session <- connect
                    (awsConfig (AWSRegion Ohio))
                    ec2Service

    putStrLn "ImportKeyPair"
    publicKeyMaterial <- KeyMaterial <$> ByteString.readFile publicKeyPath
    doImportKeyPairIfNotExists keyName publicKeyMaterial ec2Session

    putStrLn "DescribeKeyPairs"
    keyNames <- doDescribeKeyPairs ec2Session
    forM_ keyNames $ \(KeyName kn) -> Text.putStrLn $ "  " <> kn

    putStrLn "CreateSecurityGroup"
    groupId@(GroupId gid) <- doCreateSecurityGroupIfNotExists mySecurityGroupName ec2Session
    Text.putStrLn $ "  " <> gid

    putStrLn "DescribeImages"
    infos <- doDescribeImages [freeImageId] ec2Session
    forM_ infos $ \(ImageId imageId, ImageDescription description) ->
        Text.putStrLn $ "  " <> imageId <> ": " <> fromMaybe "(no description)" description

    putStrLn "RunInstances"
    (ReservationId rid, instId@(InstanceId iid)) <- doRunInstance freeImageId keyName groupId ec2Session
    Text.putStrLn $ "  Reservation ID: " <> rid
    Text.putStrLn $ "  Instance ID: " <> iid

    putStrLn "doWaitUntilInstanceRunning (please be patient!)"
    doWaitUntilInstanceRunning instId ec2Session

    putStrLn "doWaitUntilInstanceStatusOK (please be patient!)"
    doWaitUntilInstanceStatusOK instId ec2Session

    putStrLn "doGetPublicDNSName"
    mbDNSName <- doGetPublicDNSName instId ec2Session
    case mbDNSName of
        Nothing -> putStrLn "(Instance has not public DNS name)"
        Just (DNSName dn) -> do
            putStrLn "Command line to connect to instance:"
            let commandLine = "ssh -i " <> Text.pack privateKeyPath <> " ec2-user@" <> dn
            Text.putStrLn commandLine

    putStrLn "Done"
