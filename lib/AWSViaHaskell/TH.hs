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
wrapAWSService varN serviceTypeName sessionTypeName = do
    serviceVarN <- newName "x"
    sessionVarN <- newName "x"
    let serviceN = mkName serviceTypeName
        sessionN = mkName sessionTypeName
        wrappedVarN = mkName $ nameBase varN ++ "Service"
        serviceD = DataD [] serviceN [] Nothing [NormalC serviceN [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Service)]] []
        sessionD = DataD [] sessionN [] Nothing [NormalC sessionN [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Session)]] []
        serviceInst = InstanceD
                        Nothing
                        []
                        (AppT (ConT ''ServiceClass) (ConT serviceN))
                        [ TySynInstD ''TypedSession (TySynEqn [ConT serviceN] (ConT sessionN))
                        , FunD 'rawService [Clause [ConP serviceN [VarP serviceVarN]] (NormalB (VarE serviceVarN)) []]
                        , ValD (VarP 'wrappedSession) (NormalB (ConE $ mkName sessionTypeName)) []
                        ]
        sessionInst = InstanceD
                        Nothing
                        []
                        (AppT (ConT ''SessionClass) (ConT sessionN))
                        [ FunD 'rawSession [Clause [ConP sessionN [VarP sessionVarN]] (NormalB (VarE sessionVarN)) []]
                        ]
        sig = SigD wrappedVarN (ConT serviceN)
        var = ValD (VarP wrappedVarN) (NormalB (AppE (ConE serviceN) (VarE $ varN))) []
    pure
        [ serviceD
        , sessionD
        , serviceInst
        , sessionInst
        , sig
        , var
        ]
