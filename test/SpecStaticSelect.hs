{-# LANGUAGE OverloadedStrings #-}

module SpecStaticSelect (unitTests) where

import Data.Text (Text)
import MassaliaSQLSelect (
    RawSelectStruct (..), structToContent, RowFunction(ArrayAgg, Row), testAssemblingOptions,
    addWhereJoinGroup, structToSubquery
  )
import Test.Tasty
import Test.Tasty.HUnit

realStructToContent = structToContent testAssemblingOptions
realStructToSubquery = structToSubquery testAssemblingOptions

unitTests =
  testGroup
    "SQL select queries (no params)"
    [ testCase "static query with aggregation and where condition" $
        assertEqual "" (realStructToContent testSimpleQuery) "SELECT array_agg(row(truck.id, truck.vehicle_id, truck.equipment)) FROM truck LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id LEFT JOIN plant ON plant.id = truck_plant.plant_id WHERE truck.deleted_at > now() AND truck.deleted_at IS NOT NULL GROUP BY plant.id ORDER BY plant.created_at LIMIT 2",
      testCase "static query with aggregation and select subquery" $
        assertEqual "" (realStructToContent testAnotherQuery) "SELECT row(work.id, ((SELECT array_agg(row(truck.id, truck.vehicle_id, truck.equipment)) FROM truck LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id LEFT JOIN plant ON plant.id = truck_plant.plant_id WHERE truck.deleted_at > now() AND truck.deleted_at IS NOT NULL AND projection.plant_id=plant.id GROUP BY plant.id ORDER BY plant.created_at LIMIT 2))) FROM work LEFT JOIN quotation ON quotation.work_id=work.id LEFT JOIN projection ON projection.quotation_id = quotation.id GROUP BY work.id, projection.plant_id ORDER BY work.created_at DESC OFFSET 10 LIMIT 10"
    ]

testSimpleQuery :: RawSelectStruct String 
testSimpleQuery =
  RawSelectStruct
    { wrapFunctionList = [ArrayAgg, Row], -- either: "row" or "array_agg", "row"
      selectPart = ["truck.id", "truck.vehicle_id", "truck.equipment"],
      fromPart = "truck",
      joinList =
        [ "LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id",
          "LEFT JOIN plant ON plant.id = truck_plant.plant_id"
        ],
      whereConditions = Just "truck.deleted_at > now() AND truck.deleted_at IS NOT NULL",
      groupByList = ["plant.id"],
      havingConditions = Nothing,
      orderByList = ["plant.created_at"],
      offsetLimit = Just (0, 2)
    }

testAnotherQuery :: RawSelectStruct String 
testAnotherQuery =
  RawSelectStruct
    { wrapFunctionList = [Row], -- either: "row" or "array_agg", "row"
      selectPart = ["work.id", "(" <> nestedQuery <> ")"],
      fromPart = "work",
      joinList =
        [ "LEFT JOIN quotation ON quotation.work_id=work.id",
          "LEFT JOIN projection ON projection.quotation_id = quotation.id"
        ],
      whereConditions = Nothing,
      groupByList = ["work.id", "projection.plant_id"],
      havingConditions = Nothing,
      orderByList = ["work.created_at DESC"],
      offsetLimit = Just (10, 10)
    }
  where
    nestedQuery = realStructToSubquery nestedFilter
    nestedFilter = addWhereJoinGroup " AND projection.plant_id=plant.id" [] [] testSimpleQuery
