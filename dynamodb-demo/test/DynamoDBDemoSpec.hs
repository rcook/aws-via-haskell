--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

module DynamoDBDemoSpec (spec) where

import           DynamoDBDemo
import           Test.Hspec

spec :: Spec
spec = do
    describe "sample" $
        it "runs" $ sample
