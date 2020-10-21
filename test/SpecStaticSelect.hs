{-# LANGUAGE OverloadedStrings #-}

module SpecStaticSelect
  ( unitTests,
  )
where

import Data.Text (Text)
import Massalia.SQLSelectStruct (
  SelectStruct(..), selectStructToQueryFormat,
  selectStructToListSubquery
  )
import Test.Tasty
import Test.Tasty.HUnit

unitTests =
  testGroup
    "SQL select queries (no params)"
    ((\(title, res, expected) -> testCase title $ assertEqual "" expected res) <$> listCase)

listCase = [
    (
      "static query with aggregation and where condition",
      selectStructToQueryFormat testSimpleQuery,
      "SELECT truck.id, truck.vehicle_id, truck.equipment FROM truck LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id LEFT JOIN plant ON plant.id = truck_plant.plant_id WHERE truck.deleted_at > now() AND truck.deleted_at IS NOT NULL GROUP BY plant.id ORDER BY plant.created_at LIMIT 2"
    ),
    (
      "static query with aggregation and select subquery",
      selectStructToQueryFormat testAnotherQuery,
      "SELECT work.id, (SELECT coalesce((array_agg(row(truck.id, truck.vehicle_id, truck.equipment)  ORDER BY plant.created_at))[0+ 1:2+0], '{}') FROM truck LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id LEFT JOIN plant ON plant.id = truck_plant.plant_id WHERE truck.deleted_at > now() AND truck.deleted_at IS NOT NULL AND plant.id = projection.plant_id GROUP BY plant.id) FROM work LEFT JOIN quotation ON quotation.work_id=work.id LEFT JOIN projection ON projection.quotation_id = quotation.id GROUP BY work.id, projection.plant_id ORDER BY work.created_at DESC OFFSET 10 LIMIT 10"
    )
  ]

testSimpleQuery :: SelectStruct Text
testSimpleQuery = mempty {
    _select = ["truck.id", "truck.vehicle_id", "truck.equipment"],
    _from = Just "truck",
    _join = [ "LEFT JOIN truck_plant ON truck_plant.truck_id=truck.id",
          "LEFT JOIN plant ON plant.id = truck_plant.plant_id"
        ],
    _where = Just "truck.deleted_at > now() AND truck.deleted_at IS NOT NULL",
    _groupBy = ["plant.id"],
    _orderBy = ["plant.created_at"],
    _offsetLimit = Just (mempty, "2")
  }

testAnotherQuery :: SelectStruct Text
testAnotherQuery = mempty {
    _select = ["work.id", linkedSubquery],
    _from = Just "work",
    _join = [ "LEFT JOIN quotation ON quotation.work_id=work.id",
          "LEFT JOIN projection ON projection.quotation_id = quotation.id"
        ],
    _groupBy = ["work.id", "projection.plant_id"],
    _orderBy = ["work.created_at DESC"],
    _offsetLimit = Just (Just "10", "10")
  }
  where
    linkedSubquery = selectStructToListSubquery rawSubquery :: Text
    rawSubquery = testSimpleQuery <> (mempty{_where=Just " AND plant.id = projection.plant_id"})
