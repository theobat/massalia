{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Text (Text)
import qualified Hasql.Connection as Connection
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import qualified Hasql.Session as Session
import Test.Tasty
import Test.Tasty.HUnit
import qualified SpecStaticSelect
import qualified SpecDynamicSelect
import qualified SpecGraphQLSelect

main :: IO ()
main = do
  -- rawConnection <- Connection.acquire connectionSettings
  -- connection <- case rawConnection of
  --   Left e -> (error $ show e)
  --   Right goodCo -> pure goodCo
  -- result <- Session.run (dynamicallyParameterizedStatement (structToSnippet testAnotherQuery) decoder) connection
  _ <- defaultMain tests
  print "ok"
  -- where
  --   connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"

tests :: TestTree
tests = testGroup "Tests" [
    SpecStaticSelect.unitTests,
    SpecDynamicSelect.unitTests,
    SpecGraphQLSelect.unitTests
  ]
