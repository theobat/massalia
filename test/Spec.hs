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
import MassaliaFilter (GQLFilterText, defaultScalarFilter)
import MassaliaSQL (SelectStruct, globalStructureToQuery)
import MassaliaSQLSelect (RawSelectStruct (..), structToSnippet, RowFunction(ArrayAgg, Row))
import MassaliaSchema.Industry.Plant (Plant, plantInitSQL)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import Test.Tasty
import Test.Tasty.HUnit
import GHC.TypeLits (Symbol)

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
tests = testGroup "Tests" [unitTests]

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
      testCase "test simple rec traverse" $
        assertEqual "" (fst testTruckAcc) " ",
      testCase "test nested query" $
        assertEqual "" (globalStructureToQuery testPlantQuery) " ",
      testCase "encode decode filter" $
        assertEqual "" (Just defaultOkFilter) qsd
      -- the following test does not hold
      --   , testCase "List comparison (same length)" $
      --       [1, 2, 3] `compare` [1,2,2] @?= LT
    ]

testSimpleQuery =
  RawSelectStruct
    { wrapFunctionList = [ArrayAgg, Row], -- either: "row" or "array_agg", "row"
      selectPart = ["truck.id", "truck.vehicle_id", "truck.equipment"],
      fromPart = "truck",
      joinList =
        [ "LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id",
          "LEFT JOIN plant ON plant.id = truck_plant.plant_id"
        ],
      whereConditions = "truck.deleted_at > now() AND truck.deleted_at IS NOT NULL",
      groupByList = ["plant.id"],
      havingConditions = "true",
      orderByList = ["plant.created_at"],
      offsetLimit = Just (0, 2)
    }
testAnotherQuery =
  RawSelectStruct
    { wrapFunctionList = [Row], -- either: "row" or "array_agg", "row"
      selectPart = ["work.id", "array_agg(row(projection.id, projection.plant_id))"],
      fromPart = "work",
      joinList =
        [ "LEFT JOIN quotation ON quotation.work_id=work.id",
          "LEFT JOIN projection ON projection.quotation_id = quotation.id"
        ],
      whereConditions = "true",
      groupByList = ["work.id"],
      havingConditions = "true",
      orderByList = ["work.created_at DESC"],
      offsetLimit = Just (10, 10)
    }

data OkFilter = OkFilter {
  okok :: Maybe (GQLFilterText "qsd")
} deriving (Generic, Show, Eq)
deriving instance FromJSON OkFilter
deriving instance ToJSON OkFilter

defaultOkFilter = OkFilter {
  okok = Nothing
}

qsd :: Maybe OkFilter
qsd = decode "{}"
