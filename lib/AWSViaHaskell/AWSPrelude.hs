{-|
Module      : AWSViaHaskell.AWSPrelude
Description : Re-exports of most commonly used Amazonka functions
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This module provides re-exports of most commonly used Amazonka functions.
-}

module AWSViaHaskell.AWSPrelude
    ( _ServiceError
    , AsError
    , Credentials(..)
    , Region(..)
    , ServiceError
    , await
    , hasCode
    , hasStatus
    , send
    , sinkBody
    , toText
    ) where

import           Network.AWS
                    ( _ServiceError
                    , AsError
                    , Credentials(..)
                    , Region(..)
                    , ServiceError
                    , await
                    , send
                    , sinkBody
                    )
import           Network.AWS.Data (toText)
import           Network.AWS.Error
                    ( hasCode
                    , hasStatus
                    )
