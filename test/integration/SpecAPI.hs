{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Morpheus.Types (GQLRequest (..))
import MassaliaMigration (findAndRunAllMigration, defaultMigrationPattern, GlobalMigrationError)
import MigrationTypes (
  MigrationPattern(..)
  )
import MassaliaSchema.TestAPI (api, apiWithoutDB)
import Protolude
import Test.Tasty ()
import Test.Tasty.HUnit ()
import Text.Pretty.Simple (pPrint)
import Massalia.HasqlExec (poolFromURLString, release)
import Massalia.HasqlConnection (URLError)

main :: IO ()
main = do
  let queryStruct = GQLRequest
        { query = "query plantList_test { plantListPaginated (first: 10, offset: 0, globalFilter: {id: {isIn: []}}) { id name } }",
          operationName = Nothing,
          variables = Nothing
        }
  resWihtoutDB <- liftIO $ apiWithoutDB queryStruct
  pPrint resWihtoutDB
  res <- runExceptT $ executionScheme
  case res of
    Left err -> pPrint err
    Right _ -> pure ()

data TestError
  = InitErrorMigration [GlobalMigrationError]
  | InitErrorURL URLError
  deriving (Show)

executionScheme :: ExceptT TestError IO ()
executionScheme = do
  withExceptT InitErrorMigration $ findAndRunAllMigration migrationConfig dbURL
  pool <- withExceptT InitErrorURL $ ExceptT $ poolFromURLString 1 (Just 10) dbURL
  let queryStruct = GQLRequest
        { query = "query plantList_test { plantListPaginated (first: 10, offset: 0) { id name } }",
          operationName = Nothing,
          variables = Nothing
        }
  res <- liftIO $ api pool queryStruct
  liftIO $ pPrint res
  liftIO $ release pool
  where
    migrationConfig = defaultMigrationPattern {basePath = "./test"}
    dbURL = "postgres://massalia_user:p@localhost:5432/massalia_test_industry"
    dbPoolSize = 1
    dbTimeoutInSec = 10
