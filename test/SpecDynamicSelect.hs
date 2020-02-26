{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SpecDynamicSelect (unitTests) where

import Data.Text (Text)
import MassaliaSQLSelect (
    RawSelectStruct (..), structToContent, RowFunction(ArrayAgg, Row), testAssemblingOptions,
    furtherQualifyWhereJoin, structToSubquery
  )
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (id)
import MassaliaFilter (GQLFilterUUID, defaultScalarFilter, GQLScalarFilter(isIn), filterFieldToQueryPart)
import Data.UUID

realStructToContent = structToContent testAssemblingOptions
realStructToSubquery = structToSubquery testAssemblingOptions

unitTests =
  testGroup
    "SQL select queries with dynamic parameters"
    [ testCase "dynamic query with aggregation and where condition" $
        assertEqual "" (realStructToContent testSimpleQuery) "SELECT row(truck.id, truck.vehicle_id) FROM truck WHERE id =ANY( '{5a7478bc-4190-44b1-86ce-206f0ca64f43}')"
    ]

testSimpleQuery :: RawSelectStruct String 
testSimpleQuery =
  RawSelectStruct
    { wrapFunctionList = [Row], -- either: "row" or "array_agg", "row"
      selectPart = ["truck.id", "truck.vehicle_id"],
      fromPart = "truck",
      joinList = [],
      whereConditions = Just renderedTruckFilter,
      groupByList = [],
      havingConditions = Nothing,
      orderByList = [],
      offsetLimit = Nothing
    }
  where
    renderedTruckFilter = filterFieldToQueryPart (id truckFilter)
    truckFilter = TruckFilter $ Just $ simpleEqFilter (fromString "5a7478bc-4190-44b1-86ce-206f0ca64f43")

data TruckFilter = TruckFilter {
  id :: Maybe (GQLFilterUUID "id")
}

simpleEqFilter maybeUUID = defaultScalarFilter { isIn = (\a -> [a]) <$> maybeUUID }
