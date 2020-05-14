{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SpecGraphQLSelect
  ( unitTests,
  )
where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text (Text)
import GraphQLMorpheusTestData (plantQuery, truckQuery)
import MassaliaSchema.Industry.Plant (Plant)
import MassaliaSchema.Industry.Truck (Truck)
import qualified SpecStaticSelect
import Massalia.SQLClass (
    SQLSelectOptions(..),
    SQLSelect (toSelectQuery)
  )
import Massalia.SQLSelectStruct (QueryAndDecoder(query), selectStructToQueryFormat)
import Massalia.UtilsGQL (defaultPaginated)
import Test.Tasty
import Test.Tasty.HUnit

defaultOpt _ = Nothing

testPlantQuery :: QueryAndDecoder Text Plant
testPlantQuery = toSelectQuery (defaultOpt "truck") plantQuery defaultPaginated

testTruckList :: QueryAndDecoder Text Truck
testTruckList = toSelectQuery (defaultOpt "plant") truckQuery defaultPaginated

-- List of (result, expected)
listCase = [
    (
      "test simple Truck query",
      selectStructToQueryFormat (query testTruckList),
      "SELECT truck.id FROM truck"
    ),
    (
      "test simple Plant->Truck query",
      selectStructToQueryFormat (query testPlantQuery),
      "SELECT plant.id, (SELECT coalesce(array_agg(row(truck.id)), '{}') FROM truck JOIN truck_plant ON truck.id=truck_plant.truck_id WHERE truck_plant.plant_id=plant.id GROUP BY plant.id) FROM plant LIMIT 10000"
    )
  ]

unitTests =
  testGroup
    "Test graphql AST to SQL query"
    $ (\(title, result, expected) -> testCase title $ assertEqual "" expected result) <$> listCase
