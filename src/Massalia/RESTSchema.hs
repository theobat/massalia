{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Massalia.RESTSchema
-- Description : A module to define schemas for REST API that can be verified
module Massalia.RESTSchema (
  RESTSchema (..), ValidateRestSchemaContext, ValidateRestSchemaError (..),
  Person (..)
) where
import Massalia.SelectionTree (JsonMassaliaTree (..), MassaliaTree (isLeaf, getName, getChildrenList))
import Data.Text (Text)
import Massalia.Utils (intercalate)
import Protolude (Foldable(foldr'), Generic)

class RESTSchema a where
  validateRestSchema :: ValidateRestSchemaContext -> a -> JsonMassaliaTree -> [ValidateRestSchemaError]

-- Each new context is added at the head of the list
type ValidateRestSchemaContext = [Text]

newtype ValidateRestSchemaError = ValidateRestSchemaError {
  vrs_errorMessage :: Text
} deriving (Eq)
instance Show ValidateRestSchemaError where
  show = show . vrs_errorMessage

validateRestSchemaLeaf :: ValidateRestSchemaContext -> a -> JsonMassaliaTree -> [ValidateRestSchemaError]
validateRestSchemaLeaf ctx _ node
  | not (isLeaf node) = [mkError ctx $ "'" <> getName node <> "' is a leaf node"]
  | otherwise         = mempty

instance RESTSchema Int where
  validateRestSchema = validateRestSchemaLeaf
instance RESTSchema Text where
  validateRestSchema = validateRestSchemaLeaf
instance RESTSchema String where
  validateRestSchema = validateRestSchemaLeaf

data Person = Person { name :: Text, age :: Int }
  deriving (Show, Eq, Generic)
instance RESTSchema Person where
  validateRestSchema ctx Person{name, age} =
    foldr' ((<>) . validate) [] . getChildrenList
    where ctx' = "Person" : ctx
          validate child | getName child == "name" = validateRestSchema ctx' name child
                         | getName child == "age" = validateRestSchema ctx' age child
                         | otherwise = [mkError ctx' $ "'" <> getName child <> "' doesn't exist"]

mkError :: ValidateRestSchemaContext -> Text -> ValidateRestSchemaError
mkError ctx message = ValidateRestSchemaError{vrs_errorMessage=formatCtx ctx <> ": " <> message}
formatCtx :: ValidateRestSchemaContext -> Text
formatCtx = intercalate "." . reverse
