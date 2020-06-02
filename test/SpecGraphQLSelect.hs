{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SpecGraphQLSelect
  ( unitTests,
  )
where

import GraphQLMorpheusTestData (plantQuery, truckQuery)
import Massalia.SQLClass
  ( SQLSelect (toSelectQuery),
    SQLRecord (fullTopSelection)
  )
import Massalia.QueryFormat (
    TextQuery,
  )
import Massalia.SQLSelectStruct (QueryAndDecoder (query), selectStructToQueryFormat)
import Massalia.UtilsGQL (defaultPaginated)
import MassaliaSchema.Industry.Plant (Plant)
import MassaliaSchema.Industry.Truck (Truck)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import Test.Tasty
import Test.Tasty.HUnit
import Protolude
import Data.Text (pack)

defaultOpt _ = Nothing

testPlantQuery :: QueryAndDecoder Text Plant
testPlantQuery = toSelectQuery (defaultOpt "truck") plantQuery defaultPaginated

testTruckList :: QueryAndDecoder Text Truck
testTruckList = toSelectQuery (defaultOpt "plant") truckQuery defaultPaginated

-- List of (result, expected)
listCase =
  [ ( "test simple Truck query",
      selectStructToQueryFormat (query testTruckList),
      "SELECT \"truck\".\"id\" FROM \"truck\" LIMIT 10000"
    ),
    ( "test simple Plant->Truck query",
      selectStructToQueryFormat (query testPlantQuery),
      "SELECT \"plant\".\"id\", (SELECT coalesce(array_agg(row(\"truck\".\"id\")), '{}') FROM \"truck\" JOIN \"truck_plant\" ON \"truck_plant\".\"truck_id\" = \"truck\".\"id\" WHERE \"truck_plant\".\"plant_id\" = \"plant\".\"id\" GROUP BY \"plant\".\"id\" LIMIT 10000) FROM \"plant\" LIMIT 10000"
    ),
    (
      "Filters type name",
      (show (fullTopSelection @TextQuery @TruckFilter @Truck "")),
      "MassaliaNode {name = \"\", children = fromList [(\"id\",MassaliaNode {name = \"id\", children = fromList []}),(\"vehicleId\",MassaliaNode {name = \"vehicleId\", children = fromList []})]}"
    ),
    (
      "Filters type name",
      (show (fullTopSelection @TextQuery @TruckFilter @Truck "")),
      "MassaliaNode {name = \"\", children = fromList [(\"id\",MassaliaNode {name = \"id\", children = fromList []}),(\"vehicleId\",MassaliaNode {name = \"vehicleId\", children = fromList []})]}"
    )
  ]

unitTests =
  testGroup
    "Test graphql AST to SQL query"
    $ (\(title, result, expected) -> testCase title $ assertEqual "" expected result) <$> listCase
