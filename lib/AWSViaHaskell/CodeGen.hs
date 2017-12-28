--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module AWSViaHaskell.CodeGen
    ( ServiceTypeName(..)
    , declareAWSService
    ) where

import           AWSViaHaskell.Classes
import           AWSViaHaskell.Types
import           Language.Haskell.TH
import           Network.AWS (Service)

newtype ServiceTypeName = ServiceTypeName String

declareAWSService :: ServiceTypeName -> String -> Name -> Q [Dec]
declareAWSService (ServiceTypeName typeName) sessionTypeName serviceObjName' = do
    vn <- newName "serviceRaw"
    vn2 <- newName "raw"
    let foo = mkName typeName
        bar = mkName sessionTypeName
        serviceRawName = serviceObjName'
        varName = mkName $ nameBase serviceObjName' ++ "Service"
        serviceDataDec = DataD [] foo [] Nothing [NormalC foo [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Service)]] []
        sessionDataDec = DataD [] bar [] Nothing [NormalC bar [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Session)]] []
        instanceDec = InstanceD
                        Nothing
                        []
                        (AppT (ConT ''ServiceClass) (ConT foo))
                        [ TySynInstD ''TypedSession (TySynEqn [ConT foo] (ConT bar))
                        , FunD 'rawService [Clause [ConP foo [VarP vn]] (NormalB (VarE vn)) []]
                        , ValD (VarP 'wrappedSession) (NormalB (ConE $ mkName sessionTypeName)) []
                        ]
        instanceDec2 = InstanceD
                        Nothing
                        []
                        (AppT (ConT ''SessionClass) (ConT bar))
                        [ FunD 'rawSession [Clause [ConP bar [VarP vn2]] (NormalB (VarE vn2)) []]
                        ]
        varSig = SigD varName (ConT foo)
        varDec = ValD (VarP varName) (NormalB (AppE (ConE foo) (VarE $ serviceRawName))) []
    pure
        [ serviceDataDec
        , sessionDataDec
        , instanceDec
        , instanceDec2
        , varSig
        , varDec
        ]
