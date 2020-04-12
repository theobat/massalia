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
import qualified SpecAPI
import MassaliaSchema.TestAPI
import MassaliaSchema.TestAPI (api)
import Data.Morpheus.Types (GQLRequest (..))

main :: IO ()
main = do
  let quer = GQLRequest {
        query = "query plantList_test { plantListPaginated (first: 10, offset: 0) { name createdAt } }",
        operationName = Nothing,
        variables = Nothing
      }
  res <- api quer
  print res
  _ <- defaultMain tests
  print "ok"
  -- where
  --   connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"

tests :: IO TestTree
tests = 
  apiTest <- pure SpecAPI.unitTests
  staticSelect <- pure SpecStaticSelect.unitTests
  dynamicSelect <- pure SpecDynamicSelect.unitTests
  graphqlSelect <- pure SpecGraphQLSelect.unitTests
  let tests = join [
    apiTest,
    staticSelect,
    dynamicSelect,
    graphqlSelect
  ]
  testGroup "Tests" <$> tests
