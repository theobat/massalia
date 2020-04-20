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
import qualified Hasql.Session as Session
import Protolude
import qualified SpecDynamicSelect
import qualified SpecGraphQLSelect
import qualified SpecStaticInsert
import qualified SpecStaticSelect
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

-- do
--   let quer = GQLRequest {
--         query = "query plantList_test { plantListPaginated (first: 10, offset: 0) { name createdAt } }",
--         operationName = Nothing,
--         variables = Nothing
--       }
--   res <- api quer
--   print res
--   -- defaultMain =<< tests
--   print "ok"
-- where
--   connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"

tests :: TestTree
tests = testGroup "Tests" testList
  where
    testList = [staticSelect, dynamicSelect, graphqlSelect, staticInsert]
    staticSelect = SpecStaticSelect.unitTests
    staticInsert = SpecStaticInsert.unitTests
    dynamicSelect = SpecDynamicSelect.unitTests
    graphqlSelect = SpecGraphQLSelect.unitTests
