--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

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

wrapAWSService 'sdb "SDBService" "SDBSession"

newtype DomainName = DomainName Text deriving Show

newtype ItemName = ItemName Text deriving Show

doCreateDomainIfNotExists :: DomainName -> SDBSession -> IO ()
doCreateDomainIfNotExists (DomainName s) = withAWS $ do
    void $ send $ createDomain s

doListDomains :: SDBSession -> IO [DomainName]
doListDomains = withAWS $ do
    result <- send $ listDomains
    return [ DomainName s | s <- result ^. ldrsDomainNames ]

doPutAttributes :: DomainName -> ItemName -> [(Text, Text)] -> SDBSession -> IO ()
doPutAttributes (DomainName sDN) (ItemName sIN) attrs = withAWS $ do
    void $ send $ putAttributes sDN sIN
                    & paAttributes .~ map (uncurry replaceableAttribute) attrs

doGetAttributes :: DomainName -> ItemName -> SDBSession -> IO [(Text, Text)]
doGetAttributes (DomainName sDN) (ItemName sIN) = withAWS $ do
    result <- send $ getAttributes sDN sIN
    return [ (attr ^. aName, attr ^. aValue) | attr <- result ^. garsAttributes ]

main :: IO ()
main = do
    let domainName = DomainName "my-domain"
        itemName = ItemName "my-item"

    -- Default port for simpledb-dev2
    sdbSession <- connect
                    (awsConfig (Local "localhost" 8080))
                    sdbService

    putStrLn "CreateDomain"
    doCreateDomainIfNotExists domainName sdbSession

    putStrLn "ListDomains"
    domainNames <- doListDomains sdbSession
    forM_ domainNames $ \dn ->
        putStrLn $ "  " <> show dn

    putStrLn "PutAttributes"
    doPutAttributes domainName itemName [ ("1", "aaa"), ("2", "bbb") ] sdbSession

    putStrLn "GetAttributes"
    attrs <- doGetAttributes domainName itemName sdbSession
    forM_ attrs $ \(k, v) ->
        Text.putStrLn $ "  " <> k <> " = " <> v
