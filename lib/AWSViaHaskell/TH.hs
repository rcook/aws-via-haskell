--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module AWSViaHaskell.TH
    ( wrapAWSService
    ) where

import           AWSViaHaskell.Classes
import           AWSViaHaskell.Types
import           Language.Haskell.TH
import           Network.AWS (Service)

{-
Example of top-level invocation:
    wrapAWSService 'dynamoDB "DDBService" "DDBSession"

This will generate boilerplate like the following:
    data DDBService = DDBService Service
    data DDBSession = DDBSession Session
    instance ServiceClass DDBService where
        type TypedSession DDBService = DDBSession
        rawService (DDBService x) = x
        wrappedSession = DDBSession
    instance SessionClass DDBSession where
        rawSession (DDBSession x) = x
    dynamoDBService :: DDBService
    dynamoDBService = DDBService dynamoDB
-}
wrapAWSService :: Name -> String -> String -> Q [Dec]
wrapAWSService varName' typeName sessionTypeName = do
    rawServiceName' <- newName "x"
    rawSessionName' <- newName "x"
    let serviceTypeName' = mkName typeName
        sessionTypeName' = mkName sessionTypeName
        wrappedVarName' = mkName $ nameBase varName' ++ "Service"
        serviceType = DataD [] serviceTypeName' [] Nothing [NormalC serviceTypeName' [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Service)]] []
        sessionType = DataD [] sessionTypeName' [] Nothing [NormalC sessionTypeName' [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Session)]] []
        serviceInstance = InstanceD
                            Nothing
                            []
                            (AppT (ConT ''ServiceClass) (ConT serviceTypeName'))
                            [ TySynInstD ''TypedSession (TySynEqn [ConT serviceTypeName'] (ConT sessionTypeName'))
                            , FunD 'rawService [Clause [ConP serviceTypeName' [VarP rawServiceName']] (NormalB (VarE rawServiceName')) []]
                            , ValD (VarP 'wrappedSession) (NormalB (ConE $ mkName sessionTypeName)) []
                            ]
        sessionInstance = InstanceD
                            Nothing
                            []
                            (AppT (ConT ''SessionClass) (ConT sessionTypeName'))
                            [ FunD 'rawSession [Clause [ConP sessionTypeName' [VarP rawSessionName']] (NormalB (VarE rawSessionName')) []]
                            ]
        sig = SigD wrappedVarName' (ConT serviceTypeName')
        var = ValD (VarP wrappedVarName') (NormalB (AppE (ConE serviceTypeName') (VarE $ varName'))) []
    pure
        [ serviceType
        , sessionType
        , serviceInstance
        , sessionInstance
        , sig
        , var
        ]
