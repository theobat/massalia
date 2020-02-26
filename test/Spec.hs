{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Text (Text)
import GraphQLMorpheusTestData (plantSelTest, truckSelTest)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import qualified Hasql.Session as Session
import MassaliaRec
import GHC.Generics (Generic)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import MassaliaFilter (GQLFilterText, defaultScalarFilter, GQLScalarFilter(isEq))
import MassaliaSQL (SelectStruct, globalStructureToQuery)
import MassaliaSQLSelect (RawSelectStruct (..), RowFunction(ArrayAgg, Row))
import MassaliaSchema.Industry.Plant (Plant, plantInitSQL)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import Test.Tasty
import Test.Tasty.HUnit
import GHC.TypeLits (Symbol)
import qualified SpecStaticSelect
import qualified SpecDynamicSelect

main :: IO ()
main = do
  -- rawConnection <- Connection.acquire connectionSettings
  -- connection <- case rawConnection of
  --   Left e -> (error $ show e)
  --   Right goodCo -> pure goodCo
  -- result <- Session.run (dynamicallyParameterizedStatement (structToSnippet testAnotherQuery) decoder) connection
  _ <- defaultMain tests
  print "ok"
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"
    decoder =
      Decoders.singleRow
        ( (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int4)
            <*> Decoders.column (Decoders.nonNullable Decoders.uuid)
            <*> Decoders.column (Decoders.nonNullable Decoders.int4)
        )

tests :: TestTree
tests = testGroup "Tests" [
    unitTests,
    SpecStaticSelect.unitTests,
    SpecDynamicSelect.unitTests
  ]

testTruckQuery :: SelectStruct () Truck
testTruckQuery = truckInitSQL truckSelTest

testPlantQuery :: SelectStruct () Plant
testPlantQuery = plantInitSQL plantSelTest

testTruckAcc :: (Text, Truck)
testTruckAcc = truckInitSQL truckSelTest

unitTests =
  testGroup
    "Unit tests"
    [ testCase "test simple select query" $
        assertEqual "" (globalStructureToQuery testTruckQuery) "SELECT row(vehicle_id, id) FROM truck ",
      testCase "encode decode filter" $
        assertEqual "" (Just defaultOkFilter) qsd
      -- the following test does not hold
      --   , testCase "List comparison (same length)" $
      --       [1, 2, 3] `compare` [1,2,2] @?= LT
    ]


data OkFilter = OkFilter {
  okok :: Maybe (GQLFilterText "qsd")
} deriving (Generic, Show, Eq, FromJSON, ToJSON)
  
defaultOkFilter :: OkFilter
defaultOkFilter = OkFilter {
  okok = Just (defaultScalarFilter {isEq = Just "whoot"})
}

qsd :: Maybe OkFilter
qsd = decode "{ \"okok\": {\"isEq\": \"whoot\"} }"
