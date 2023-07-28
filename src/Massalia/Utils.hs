{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

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
    ZonedTime,
    ZonedTimeEq(ZonedTimeEq),
    toZonedTime,
    zonedTimeEqToUTC,
    Day,
    TimeOfDay,
    TimeZone,
    UTCTime,
    localTimeDefault,
    utcTimeDefault,
    dayDefault,
    -- postgres numbers
    Scientific,
    -- PrettyPrint
    pPrint,
    -- String
    simpleSnakeCase,
    simpleSnakeCaseT,
    unsafeSnakeCaseT,
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
import Data.Time (
    LocalTime,
    Day,
    TimeOfDay,
    TimeZone,
    UTCTime
  )
import Data.Scientific (Scientific)
import Data.Time (ZonedTime, zonedTimeToUTC, UTCTime(UTCTime), secondsToDiffTime)
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
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Char as Char

-- --------------- TEXT
intercalate :: (Monoid a, Foldable t) => a -> t a -> a
intercalate = intercalateMap identity

-- | 
-- >>> intercalateMap @Int @Text show ", " [1, 2, 3]
-- "1, 2, 3"
-- >>> intercalateMap @Int @Text show ", " [1]
-- "1"
-- >>> intercalateMap @Int @Text show ", " []
-- ""
intercalateMap ::
  forall sourceType targetMonoid container.
  (Monoid targetMonoid, Foldable container) =>
  (sourceType -> targetMonoid) -> targetMonoid -> container sourceType -> targetMonoid
intercalateMap mapper separator currentList = case res of
  (_, r) -> r
  where
    res = foldr' iterator (0 :: Int, mempty) currentList
    iterator !element !tuple = case tuple of
      (0, !acc) -> (1, mapper element <> acc)
      (!i, !acc) -> (i + 1, mapper element <> separator <> acc)


toCSVInParens :: (IsString a, Monoid a) => [a] -> a
toCSVInParens input = inParens $ intercalate "," input

inParens :: (Semigroup a, IsString a) => a -> a
inParens a = "(" <> a <> ")"
stringToText :: String -> Text
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
dayDefault = ModifiedJulianDay 0

utcTimeDefault = UTCTime dayDefault (secondsToDiffTime 0)

unsafeSnakeCaseT :: Text -> Text
unsafeSnakeCaseT t = case toUnderscore t of
  Left _ -> t
  Right r -> r

-- | A very simple snake_case converter. It's using 'String' so
-- unless you're doing type level programming it's not what you want.
simpleSnakeCase :: String -> String
simpleSnakeCase = foldl' iterator baseCase
  where
    baseCase :: String
    baseCase = ""
    iterator :: String -> Char -> String
    iterator "" c = [Char.toLower c]
    iterator name c
      | Char.isUpper c = name ++ "_" ++ [Char.toLower c]
      | otherwise = name ++ [Char.toLower c]

-- | A very simple snake_case converter.
simpleSnakeCaseT :: Text -> Text
simpleSnakeCaseT = Text.concatMap iterator
  where
    iterator c
      | Char.isUpper c = "_" <> lowerChar c
      | otherwise = lowerChar c
    lowerChar c = Text.singleton $ Char.toLower c

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

-- | A simple wrapper around 'ZonedTime' with a utc centric notion of equality
-- (that is, we ignore the timezones to check for equality).
--
-- >>> let ex1 = ZonedTimeEq <$> readMaybe "2002-12-12 12:12:12+0000"
-- >>> let ex2 = ZonedTimeEq <$> readMaybe "2002-12-12 02:12:12-1000"
-- >>> (ex1, ex2, ex1 == ex2)
-- (Just 2002-12-12 12:12:12 +0000,Just 2002-12-12 02:12:12 -1000,True)
newtype ZonedTimeEq = ZonedTimeEq ZonedTime
  deriving stock (Generic)

instance Eq ZonedTimeEq where
  (==) a b = zonedTimeEqToUTC a == zonedTimeEqToUTC b
toZonedTime :: ZonedTimeEq -> ZonedTime
toZonedTime (ZonedTimeEq a) = a
zonedTimeEqToUTC :: ZonedTimeEq -> UTCTime
zonedTimeEqToUTC = zonedTimeToUTC . toZonedTime
deriving via ZonedTime instance Show ZonedTimeEq
deriving via ZonedTime instance FromJSON ZonedTimeEq
deriving via ZonedTime instance ToJSON ZonedTimeEq
