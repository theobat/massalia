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

data OutMapper haskellType fieldType sqlDecoderType = OutMapper {
    sqlFieldName :: Text,
    updater :: haskellType -> fieldType -> haskellType,
    sqlDecoder :: sqlDecoderType fieldType
}