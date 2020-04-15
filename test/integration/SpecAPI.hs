{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude
import qualified Hasql.Connection as Connection
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import qualified Hasql.Session as Session
import Test.Tasty
import Test.Tasty.HUnit
import qualified SpecStaticSelect
import qualified SpecDynamicSelect
import qualified SpecGraphQLSelect
import MassaliaSchema.TestAPI
import MassaliaSchema.TestAPI (api)
import Data.Morpheus.Types (GQLRequest (..))
import qualified Hasql.Transaction                    as Tx
import qualified Hasql.Transaction.Sessions           as Txs
import Hasql.Migration (runMigration, loadMigrationFromFile, MigrationCommand ( MigrationInitialization ))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  let connectionSettings = Connection.settings "localhost" 5432 "massalia_user" "p" "massalia_test_industry"
  connectionOrError <- Connection.acquire connectionSettings
  connection <- case connectionOrError of
    Left e -> panic $ show e
    Right goodCo -> pure goodCo
  r <- runTx connection $ runMigration $ MigrationInitialization
  migrationFile <- loadMigrationFromFile "schema.sql" "./test/MassaliaSchema/schema.sql"
  res <- runTx connection $ runMigration migrationFile
  pPrint res
  runTx connection (Tx.sql "SELECT 1")
  runTx connection (Tx.sql "SELECT 'this is HASQL !!'")
  Connection.withLibPQConnection connection (const $ defaultMain unitTests) 
  Connection.release connection

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