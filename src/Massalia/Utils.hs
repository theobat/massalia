{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
    emailValidateText,
    emailToByteString,
    emailToText,
    emailDefault,
    -- Ranges
    SimpleRange(..),
    Inclusivity(..),
    defaultSimpleRange,
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
    localTimeDefault,
    -- postgres numbers
    Scientific,
    -- PrettyPrint
    pPrint,
    -- String
    simpleSnakeCase,
    inParens
  )
where

import Data.Aeson (FromJSON, ToJSON)

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
import Data.Time.LocalTime (LocalTime(LocalTime), midnight)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Protolude hiding (intercalate)
import Text.Email.Validate (EmailAddress)
import Text.Email.QuasiQuotation (email)
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
emailValidateText = (first pack) . EmailAddress.validate

emailToByteString :: EmailAddress -> ByteString
emailToByteString = EmailAddress.toByteString
emailToText = decodeUtf8 . EmailAddress.toByteString
emailDefault = [email|default@default.com|]

--------------- Time/Day/Timestamp

localTimeDefault = LocalTime dayDefault midnight  
dayDefault = (ModifiedJulianDay 0)  

-- | A very simple snake_case converter. It's using 'Text' and
-- folds over the characters to lower them and add an @"_"@ prefix
-- if the char is Upper case.
naiveSnake :: Text -> Text
naiveSnake = undefined -- TODO

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

-- | A very simple, JSON oriented, representation for a range.
-- A null @start@ means @-infinity@ and a null end means @+infinity@.
-- A null bound (start or end) is always considered excluded
-- (no matter what the inclusivity says)
data SimpleRange a = SimpleRange {
  start :: Maybe a,
  end :: Maybe a,
  inclusivity :: Maybe Inclusivity
} deriving (Eq, Show, Generic, FromJSON, ToJSON)
data Inclusivity = II | IE | EI | EE deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A very simple, JSON oriented, representation for a range.
defaultSimpleRange :: SimpleRange a
defaultSimpleRange = SimpleRange { start = Nothing, end = Nothing, inclusivity = Nothing }
