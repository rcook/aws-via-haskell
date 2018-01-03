{-|
Module      : Main
Description : EC2 demo
Copyright   : (C) Richard Cook, 2018
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
                    ( Endpoint(..)
                    , awsConfig
                    , connect
                    , withAWS
                    , wrapAWSService
                    )
import           Control.Monad (void)
import           Data.Text (Text)
import           Network.AWS
                    ( Region(..)
                    , send
                    )
import           Network.AWS.EC2
                    ( ec2
                    , runInstances
                    )

wrapAWSService 'ec2 "EC2Service" "EC2Session"

newtype ImageId = ImageId Text deriving Show

doRunInstances :: ImageId -> EC2Session -> IO ()
doRunInstances (ImageId iid) = withAWS $
    void $ send $ runInstances iid 1 1

main :: IO ()
main = do
    ec2Session <- connect
                    (awsConfig (AWSRegion Ohio))
                    ec2Service

    putStrLn "RunInstances"
    let imageId = ImageId "foobar"
    doRunInstances imageId ec2Session

    putStrLn "Done"
