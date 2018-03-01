{-|
Module      : Main
Description : Imports module for SSM demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module SSMImports
    ( ParameterType(..)
    , getParameters
    , grsParameters
    , pValue
    , pVersion
    , ppOverwrite
    , putParameter
    , ssm
    ) where

import Network.AWS.SSM
