--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module AWSViaHaskell.Types
    ( Session(..)
    , sEnv
    , sRegion
    , sService
    ) where

import           Control.Lens (makeLenses)
import           Network.AWS (Env, Region, Service)

data Session = Session
    { _sEnv :: Env
    , _sRegion :: Region
    , _sService :: Service
    }
makeLenses ''Session
