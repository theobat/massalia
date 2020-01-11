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
    ) where

import Data.Text
import Data.UUID
import qualified Hasql.Decoders as Decoders


data OutMapper recordType fieldType sqlDecoderType = OutMapper {
    sqlExpr :: Text,
    updater :: recordType -> fieldType -> recordType,
    acccessor :: recordType -> fieldType,
    sqlDecoder :: sqlDecoderType fieldType
}

-- TODO all the others:

type OutMapperUUID recordType = OutMapper recordType UUID Decoders.Value
uuidMapper fieldName updater accessor = OutMapper {
    sqlExpr = fieldName,
    updater = updater,
    acccessor = accessor,
    sqlDecoder = Decoders.uuid
}

type OutMapperText recordType = OutMapper recordType Text Decoders.Value
textMapper fieldName updater accessor = OutMapper {
    sqlExpr = fieldName,
    updater = updater,
    acccessor = accessor,
    sqlDecoder = Decoders.text
}
