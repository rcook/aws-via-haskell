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
    ( module AWSViaHaskell.AWSService
    , module AWSViaHaskell.Classes
    , module AWSViaHaskell.TH
    , module AWSViaHaskell.Types
    , module AWSViaHaskell.Util
    ) where

import           AWSViaHaskell.AWSService
import           AWSViaHaskell.Classes
import           AWSViaHaskell.TH
import           AWSViaHaskell.Types
import           AWSViaHaskell.Util
