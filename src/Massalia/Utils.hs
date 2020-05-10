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
    toCSVInParens,
    -- EmailAddress
    EmailAddress,
    emailValidate,
    emailToByteString,
    emailToText,
    -- UUID
    UUID.UUID,
    uuidNil,
    uuidToText,
    uuidFromText,
    uuidFromString,
    uuidV4,
    -- Time/Date
    LocalTime,
    Day,
    TimeOfDay,
    TimeZone,
    UTCTime,
    -- postgres numbers
    Scientific,
    -- PrettyPrint
    pPrint,
    -- String
    simpleSnakeCase
  )
where

-- UUID

-- Text
import Data.Text (pack)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
-- Email

-- LocalTime
import PostgreSQL.Binary.Data (
    LocalTime,
    Day,
    TimeOfDay,
    TimeZone,
    UTCTime,
    Scientific
  )
import Protolude hiding (intercalate)
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress
import Text.Inflections (toUnderscore)
-- pretty print
import Text.Pretty.Simple (pPrint)

-- Legacy String
import Data.String (String, IsString(fromString))
import Data.Char (isUpper, toLower)


-- --------------- TEXT
intercalate :: Monoid a => a -> [a] -> a
intercalate = intercalateMap identity

intercalateMap :: Monoid a => (b -> a) -> a -> [b] -> a
intercalateMap mapper separator currentList = case currentList of
  [] -> mempty
  [e1] -> mapper e1
  (e1 : reducedList) -> mapper e1 <> separator <> intercalateMap mapper separator reducedList


toCSVInParens :: (IsString a, Monoid a) => [a] -> a
toCSVInParens list = inParens $ intercalate "," list

inParens a = "(" <> a <> ")"
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
emailToText = decodeUtf8 . EmailAddress.toByteString

-- | A very simple snake_case converter. It's using 'String' so
-- unless you're doing type level programming it's not what you want.
simpleSnakeCase :: String -> String
simpleSnakeCase = foldl' iterator baseCase
  where
    baseCase :: String
    baseCase = ""
    iterator :: String -> Char -> String
    iterator "" c = [toLower c]
    iterator name c
      | isUpper c = name ++ "_" ++ [toLower c]
      | otherwise = name ++ [toLower c]
    