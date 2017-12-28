--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

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
