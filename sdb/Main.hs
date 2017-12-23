--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           AWSViaHaskell
                    ( AWSAction
                    , AWSConfig(..)
                    , AWSConnection
                    , LoggingState(..)
                    , ServiceEndpoint(..)
                    , awsConfig
                    , getAWSConnection
                    , withAWS
                    )
import           Control.Lens ((&), (^.), (.~))
import           Control.Monad (forM_, void)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Network.AWS (send)
import           Network.AWS.SDB
                    ( aName
                    , aValue
                    , createDomain
                    , garsAttributes
                    , getAttributes
                    , ldrsDomainNames
                    , listDomains
                    , paAttributes
                    , putAttributes
                    , replaceableAttribute
                    , sdb
                    )

newtype DomainName = DomainName Text deriving Show

newtype ItemName = ItemName Text deriving Show

doCreateDomainIfNotExists :: DomainName -> AWSAction ()
doCreateDomainIfNotExists (DomainName s) = withAWS $ do
    void $ send $ createDomain s

doListDomains :: AWSAction [DomainName]
doListDomains = withAWS $ do
    result <- send $ listDomains
    return [ DomainName s | s <- result ^. ldrsDomainNames ]

doPutAttributes :: DomainName -> ItemName -> [(Text, Text)] -> AWSAction ()
doPutAttributes (DomainName sDN) (ItemName sIN) attrs = withAWS $ do
    void $ send $ putAttributes sDN sIN
                    & paAttributes .~ map (uncurry replaceableAttribute) attrs

doGetAttributes :: DomainName -> ItemName -> AWSAction [(Text, Text)]
doGetAttributes (DomainName sDN) (ItemName sIN) = withAWS $ do
    result <- send $ getAttributes sDN sIN
    return [ (attr ^. aName, attr ^. aValue) | attr <- result ^. garsAttributes ]

main :: IO ()
main = do
    -- Default port for simpledb-dev2
    awsInfo <- getAWSConnection $ (awsConfig (Local "localhost" 8080) sdb)
                                    { acLoggingState = LoggingDisabled }

    let domainName = DomainName "my-domain"
        itemName = ItemName "my-item"

    putStrLn "CreateDomain"
    doCreateDomainIfNotExists domainName awsInfo

    putStrLn "ListDomains"
    domainNames <- doListDomains awsInfo
    forM_ domainNames $ \dn ->
        putStrLn $ "  " <> show dn

    putStrLn "PutAttributes"
    doPutAttributes domainName itemName [ ("1", "aaa"), ("2", "bbb") ] awsInfo

    putStrLn "GetAttributes"
    attrs <- doGetAttributes domainName itemName awsInfo
    forM_ attrs $ \(k, v) ->
        Text.putStrLn $ "  " <> k <> " = " <> v
