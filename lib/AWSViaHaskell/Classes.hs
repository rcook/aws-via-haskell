{-|
Module      : AWSViaHaskell.Classes
Description : Service and session type classes
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This modules provides service and session type classes for the "AWS via Haskell" project.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module AWSViaHaskell.Classes
    ( ServiceClass(..)
    , SessionClass(..)
    ) where

import           AWSViaHaskell.Types
import           Network.AWS (Service)

class ServiceClass a where
    type TypedSession a :: *
    rawService :: a -> Service
    wrappedSession :: Session -> TypedSession a

class SessionClass a where
    rawSession :: a -> Session
