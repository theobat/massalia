{-# LANGUAGE PartialTypeSignatures #-}

-- |
-- Module      : Massalia.HasqlExec
-- Description : A module to reexport all the Execution layer stuff into a single module.
-- Reexports everything from "Hasql.Pool", "Hasql.Session", "Hasql.Statement".
module Massalia.HasqlExec
  ( module Hasql.Pool,
    module Hasql.Session,
    module Hasql.Statement,
    dynamicallyParameterizedStatement,
    dynamicallyParameterized,
    eitherURLToPool,
    poolFromURLString
  )
where

import Hasql.Pool
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Session
import Hasql.Statement
import Massalia.HasqlConnection (settingsFromURL, URLError(Malformed))
import Data.Time (NominalDiffTime)

poolFromURLString :: Int -> NominalDiffTime -> String -> IO (Either URLError Pool)
poolFromURLString poolSize timeout url = eitherURLToPool poolSize timeout $ settingsFromURL url

eitherURLToPool :: Int -> NominalDiffTime -> Either URLError _ -> IO (Either URLError Pool)
eitherURLToPool poolSize timeout input = case input of
  Left err -> pure $ Left err
  Right settings -> Right <$> (acquire (poolSize, timeout, settings))