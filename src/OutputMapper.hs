{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module OutputMapper
    ( OutMapper(..)
    , uuidMapper
    , textMapper
    ) where

import Data.Text
import Data.UUID
import qualified Hasql.Decoders as Decoders


data OutMapper recordType = OutMapper {
    sqlExpr :: Text,
    fieldMapper :: FieldOutMapper recordType
}
data FieldOutStruct recordType fieldType = FieldOutStruct {
    updater :: recordType -> fieldType -> recordType,
    acccessor :: recordType -> fieldType,
    sqlDecoder :: Decoders.Value fieldType 
}
data FieldOutMapper recordType =
    FieldOutUUID (FieldOutStruct recordType UUID) |
    FieldOutBool (FieldOutStruct recordType Bool) |
    FieldOutText (FieldOutStruct recordType Text) |
    FieldOutRec recordType

-- TODO all the others:
uuidMapper :: Text -> (recordType -> UUID -> recordType) -> (recordType -> UUID) -> OutMapper recordType
uuidMapper fieldName updater accessor = OutMapper {
    sqlExpr = fieldName,
    fieldMapper = FieldOutUUID $ FieldOutStruct {
        updater = updater,
        acccessor = accessor,
        sqlDecoder = Decoders.uuid
    }
}

textMapper :: Text -> (recordType -> Text -> recordType) -> (recordType -> Text) -> OutMapper recordType
textMapper fieldName updater accessor = OutMapper {
    sqlExpr = fieldName,
    fieldMapper = FieldOutText $ FieldOutStruct {
        updater = updater,
        acccessor = accessor,
        sqlDecoder = Decoders.text
    }
}
