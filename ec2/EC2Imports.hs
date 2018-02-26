{-|
Module      : EC2Imports
Description : Imports module for EC2 demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module EC2Imports
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
    , diiInstanceIds
    , diirsImages
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
    ) where

import Network.AWS.EC2
