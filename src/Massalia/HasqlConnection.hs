{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.HasqlConnection
-- Description : Reexports everything from "Hasql.Connection".
  
module Massalia.HasqlConnection
  ( module Connection,
    URLError(Malformed),
    InitDBConnectionError(..),
    safeSettingsToConnection,
    releasePotentialConnection,
    settingsFromURL,
    connectionFromURL
  )
where

-- import Hasql.Connection

import Hasql.URL (parseDatabaseUrl)
import Hasql.Connection as Connection
import Protolude
import Data.String

data URLError = Malformed deriving (Eq, Show)
data InitDBConnectionError
  = ConnectionURLError URLError
  | HasqlConnectionError ConnectionError
  deriving (Eq, Show)

connectionFromURL :: String -> IO (Either InitDBConnectionError Connection)
connectionFromURL url = safeSettingsToConnection $ first ConnectionURLError $ settingsFromURL url

settingsFromURL :: String -> Either URLError Settings
settingsFromURL inputURL = maybeToRight Malformed $ (parseDatabaseUrl inputURL)

safeSettingsToConnection :: Either InitDBConnectionError Settings -> IO (Either InitDBConnectionError Connection)
safeSettingsToConnection maybeSettings = case maybeSettings of
  Left err -> pure $ Left err
  Right realSettings -> first HasqlConnectionError <$> Connection.acquire realSettings

releasePotentialConnection :: Either a Connection -> IO (Either a ())
releasePotentialConnection conn = case conn of
  Left err -> pure $ Left err
  Right currentConn -> Right <$> Connection.release currentConn
