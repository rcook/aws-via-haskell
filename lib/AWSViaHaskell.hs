{-|
Module      : AWSViaHaskell
Description : Umbrella module
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module AWSViaHaskell
    ( module AWSViaHaskell.Classes
    , module AWSViaHaskell.Service
    , module AWSViaHaskell.TH
    , module AWSViaHaskell.Types
    , module AWSViaHaskell.Util
    ) where

import           AWSViaHaskell.Classes
import           AWSViaHaskell.Service
import           AWSViaHaskell.TH
import           AWSViaHaskell.Types
import           AWSViaHaskell.Util
