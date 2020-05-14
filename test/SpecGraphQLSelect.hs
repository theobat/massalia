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
import qualified SpecDynamicSelect
import qualified SpecStaticSelect
import Massalia.SQLClass (
    SQLSelectOptions(..),
    SQLSelect (toSelectQuery)
  )
import Massalia.SQLSelect (SelectStruct, testAssemblingOptions, selectStructToContent)
import Massalia.UtilsGQL (defaultPaginated)
import Test.Tasty
import Test.Tasty.HUnit

defaultOpt _ = Nothing

testPlantQuery :: SelectStruct Plant Text
testPlantQuery = toSelectQuery (defaultOpt "truck") plantQuery defaultPaginated

testTruckList :: SelectStruct Truck Text
testTruckList = toSelectQuery (defaultOpt "plant") truckQuery defaultPaginated

-- List of (result, expected)
listCase = [
    (
      "test simple Truck query",
      selectStructToContent testAssemblingOptions testTruckList,
      "SELECT row(truck.id) FROM truck"
    )
    -- (
    --   "test simple Plant->Truck query",
    --   selectStructToContent testAssemblingOptions testPlantQuery,
    --   "SELECT row((SELECT array_agg(row(truck.id)) FROM truck JOIN truck_plant ON truck.id=truck_plant.truck_id WHERE truck_plant.plant_id=plant.id GROUP BY plant.id), plant.id) FROM plant LIMIT 20"
    -- )
  ]

unitTests =
  testGroup
    "Test graphql AST to SQL query"
    $ (\(title, result, expected) -> testCase title $ assertEqual "" expected result) <$> listCase
