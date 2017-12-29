{-|
Module      : AWSViaHaskell.Util
Description : General-purpose helper functions
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This modules provides general-purpose helper functions for the "AWS via Haskell" project.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module AWSViaHaskell.Util
    ( intToText
    , parseInt
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (null, pack)
import qualified Data.Text.Read as Text (decimal)

intToText :: Int -> Text
intToText = Text.pack . show

parseInt :: Text -> Maybe Int
parseInt s = case Text.decimal s of
    Left _ -> Nothing
    Right (result, s') -> if Text.null s' then Just result else Nothing
