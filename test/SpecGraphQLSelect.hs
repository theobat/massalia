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
import Massalia.SQLSelectStruct (QueryAndDecoder (query), selectStructToQueryFormat)
import Massalia.UtilsGQL (Paginated, defaultPaginated, globalFilter, unionFilter)
import MassaliaSchema.Industry.Plant (Plant)
import MassaliaSchema.Industry.Truck (Truck)
import MassaliaSchema.Industry.TruckFilter (TruckFilter)
import MassaliaSchema.Industry.PlantFilter (PlantFilter, plantFilterTest)
import Test.Tasty
import Test.Tasty.HUnit
import Protolude

testPlantQuery :: QueryAndDecoder Text Plant
testPlantQuery = toSelectQuery plantQuery (defaultPaginated @PlantFilter)
testFilteredPlantQuery :: QueryAndDecoder Text Plant
testFilteredPlantQuery = toSelectQuery plantQuery (defaultPaginated{globalFilter = Just plantFilterTest})
testUnionFilterPlantQuery :: QueryAndDecoder Text Plant
testUnionFilterPlantQuery = toSelectQuery plantQuery (defaultPaginated{
    unionFilter = Just [plantFilterTest, plantFilterTest]
  })

testTruckList :: QueryAndDecoder Text Truck
testTruckList = toSelectQuery truckQuery (defaultPaginated @TruckFilter)

-- List of (result, expected)
listCase :: [(TestName, Text, Text)]
listCase =
  [ ( "test simple Truck query",
      selectStructToQueryFormat (query testTruckList),
      "SELECT \"truck\".\"id\" FROM \"truck\""
    ),
    ( "test simple Plant->Truck query",
      selectStructToQueryFormat (query testPlantQuery),
      "SELECT \"plant\".\"id\", coalesce((SELECT coalesce((array_agg(row(\"truck\".\"id\") )), '{}') FROM \"truck\" JOIN \"truck_plant\" ON \"truck_plant\".\"truck_id\" = \"truck\".\"id\" WHERE \"truck_plant\".\"plant_id\" = \"plant\".\"id\" GROUP BY \"plant\".\"id\"), '{}') FROM \"plant\""
    ),
    ( "test simple Plant->Truck query (filtered)",
      selectStructToQueryFormat (query testFilteredPlantQuery),
      "SELECT \"plant\".\"id\", coalesce((SELECT coalesce((array_agg(row(\"truck\".\"id\") )), '{}') FROM \"truck\" JOIN \"truck_plant\" ON \"truck_plant\".\"truck_id\" = \"truck\".\"id\" WHERE \"truck_plant\".\"plant_id\" = \"plant\".\"id\" GROUP BY \"plant\".\"id\"), '{}') FROM \"plant\" WHERE (\"plant\".\"check_date\" <@ daterange('1991-08-21',null))"
    ),
    ( "test simple Plant->Truck query (union filter)",
      selectStructToQueryFormat (query testUnionFilterPlantQuery),
      "SELECT \"plant\".\"id\", coalesce((SELECT coalesce((array_agg(row(\"truck\".\"id\") )), '{}') FROM \"truck\" JOIN \"truck_plant\" ON \"truck_plant\".\"truck_id\" = \"truck\".\"id\" WHERE \"truck_plant\".\"plant_id\" = \"plant\".\"id\" GROUP BY \"plant\".\"id\"), '{}') FROM ((SELECT \"plant\".* FROM \"plant\" WHERE (\"plant\".\"check_date\" <@ daterange('1991-08-21',null))) UNION (SELECT \"plant\".* FROM \"plant\" WHERE (\"plant\".\"check_date\" <@ daterange('1991-08-21',null))))\"plant\""
    ),
    (
      "Filters type name",
      (show (fullTopSelection @(Paginated TruckFilter) @Truck "")),
      "MassaliaNode {name = \"\", children = fromList [(\"id\",MassaliaNode {name = \"id\", children = fromList []}),(\"vehicleId\",MassaliaNode {name = \"vehicleId\", children = fromList []})]}"
    ),
    (
      "Filters type name",
      (show (fullTopSelection @(Paginated TruckFilter) @Truck "")),
      "MassaliaNode {name = \"\", children = fromList [(\"id\",MassaliaNode {name = \"id\", children = fromList []}),(\"vehicleId\",MassaliaNode {name = \"vehicleId\", children = fromList []})]}"
    )
  ]

unitTests :: TestTree
unitTests =
  testGroup
    "Test graphql AST to SQL query"
    $ (\(title, result, expected) -> testCase title $ assertEqual "" expected result) <$> listCase
