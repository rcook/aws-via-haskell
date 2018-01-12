{-|
Module      : Main
Description : Imports module for DynamoDB demo
Copyright   : (C) Richard Cook, 2017
License     : MIT
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable
-}

module DynamoDBImports
    ( _ResourceInUseException
    , _ResourceNotFoundException
    , KeyType(..)
    , ScalarAttributeType(..)
    , attributeDefinition
    , attributeValue
    , avN
    , avS
    , createTable
    , ctAttributeDefinitions
    , deleteTable
    , describeTable
    , dynamoDB
    , getItem
    , giKey
    , girsItem
    , keySchemaElement
    , piItem
    , provisionedThroughput
    , putItem
    , tableExists
    , tableNotExists
    , uiExpressionAttributeValues
    , uiKey
    , uiUpdateExpression
    , updateItem
    ) where

import Network.AWS.DynamoDB
