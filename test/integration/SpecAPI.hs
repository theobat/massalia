{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Morpheus.Types (GQLRequest (..))
import qualified Hasql.Connection as Connection
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import Hasql.Migration (MigrationCommand (MigrationInitialization), loadMigrationFromFile, runMigration)
import Massalia.Migration (findAndRunAllMigration, MigrationPattern(..), defaultMigrationPattern) 
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import MassaliaSchema.TestAPI (api)
import Protolude
import qualified SpecDynamicSelect
import qualified SpecGraphQLSelect
import qualified SpecStaticSelect
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  res <- runExceptT $ findAndRunAllMigration migrationConfig dbURL
  case res of
    Left err -> pPrint err
    Right _ -> pure ()
  where
    migrationConfig = defaultMigrationPattern {basePath = "./test"}
    dbURL = "postgres://massalia_user:p@localhost:5432/massalia_test_industry"

-- main :: IO ()
-- main = do
--   let connectionSettings = Connection.settings "localhost" 5432 "massalia_user" "p" "massalia_test_industry"
--   connectionOrError <- Connection.acquire connectionSettings
--   connection <- case connectionOrError of
--     Left e -> panic $ show e
--     Right goodCo -> pure goodCo
--   r <- runTx connection $ runMigration $ MigrationInitialization
--   -- migrationFile <- loadMigrationFromFile "schema.sql" "./test/MassaliaSchema/schema.sql"
--   -- res <- runTx connection $ runMigration migrationFile
--   -- pPrint res
--   runTx connection (Tx.sql "SELECT 1")
--   runTx connection (Tx.sql "SELECT 'this is HASQL !!'")
--   let quer =
--         GQLRequest
--           { query = "query plantList_test { plantListPaginated (first: 10, offset: 0) { id name } }",
--             operationName = Nothing,
--             variables = Nothing
--           }
--   res <- api connection quer
--   pPrint res
--   -- Connection.withLibPQConnection connection (const $ defaultMain unitTests)
--   Connection.release connection

unitTests :: TestTree
unitTests =
  testGroup
    "GQL API plantList Simple"
    [ testCase "dummy query" $
        assertEqual "" "" ""
    ]

runTx :: Connection.Connection -> Tx.Transaction a -> IO (Either Session.QueryError a)
runTx con act = do
  Session.run (Txs.transaction Txs.ReadCommitted Txs.Write act) con
