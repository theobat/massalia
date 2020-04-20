{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.Utils
-- Description : A bunch of reexports from useful libraries such as EmailValidation or UUID
module Massalia.Utils
  ( -- TEXT
    Text,
    intercalate,
    intercalateMap,
    stringToText,
    toUnderscore,
    -- EmailAddress
    EmailAddress,
    emailValidate,
    emailToByteString,
    -- UUID
    UUID.UUID,
    uuidNil,
    uuidToText,
    uuidFromText,
    uuidFromString,
    uuidV4,
    -- LocalTime
    LocalTime,
    -- PrettyPrint
    pPrint,
  )
where

-- UUID

-- Text
import Data.Text (pack)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
-- Email

-- LocalTime
import PostgreSQL.Binary.Data (LocalTime)
import Protolude hiding (intercalate)
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress
import Text.Inflections (toUnderscore)
-- pretty print
import Text.Pretty.Simple (pPrint)

-- --------------- TEXT
intercalate :: Monoid a => a -> [a] -> a
intercalate = intercalateMap identity

intercalateMap :: Monoid a => (b -> a) -> a -> [b] -> a
intercalateMap mapper separator currentList = case currentList of
  [] -> mempty
  [e1] -> mapper e1
  (e1 : reducedList) -> mapper e1 <> separator <> intercalateMap mapper separator reducedList

stringToText = pack

-- --------------- UUID
uuidNil = UUID.nil

uuidToText = UUID.toText

uuidFromText = UUID.fromText

uuidFromString = UUID.fromString

uuidV4 = nextRandom

--------------------- EmailAddress
emailValidate = EmailAddress.validate

emailToByteString = EmailAddress.toByteString
