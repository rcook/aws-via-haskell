--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

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
