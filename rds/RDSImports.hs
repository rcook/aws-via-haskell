{-|
Module      : Main
Description : Imports module for RDS demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module RDSImports
    ( cdiAllocatedStorage
    , cdiMasterUserPassword
    , cdiMasterUsername
    , cdirsDBInstance
    , createDBInstance
    , diDBInstanceARN
    , diEndpoint
    , diEngineVersion
    , eAddress
    , ePort
    , rds
    ) where

import Network.AWS.RDS
