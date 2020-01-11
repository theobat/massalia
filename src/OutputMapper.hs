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
import qualified Hasql.Decoders as Decoders


data OutMapper recordType fieldType sqlDecoderType = OutMapper {
    sqlFieldName :: Text,
    updater :: recordType -> fieldType -> recordType,
    sqlDecoder :: sqlDecoderType fieldType
}

type OutMapperUUID = OutMapper recordType UUID Decoders.uuid
type OutMapperText = OutMapper recordType Text Decoders.text
