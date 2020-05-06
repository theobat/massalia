{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SpecDynamicSelect
  ( unitTests,
  )
where

import Data.Text (Text)
import Data.UUID
import Massalia.Filter (GQLFilterUUID, GQLScalarFilter (isIn), defaultScalarFilter, filterFieldToMaybeQueryPart)
import Massalia.SQLSelect
  ( AssemblingOptions,
    RawSelectStruct (..),
    RowFunction (ArrayAgg, Row),
    structToContent,
    structToSubquery,
    testAssemblingOptions,
  )
import Massalia.Utils (Text, UUID, uuidFromString)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (id)

realStructToContent = structToContent (testAssemblingOptions :: AssemblingOptions Text)

realStructToSubquery = structToSubquery (testAssemblingOptions :: AssemblingOptions Text)

unitTests =
  testGroup
    "SQL select queries with dynamic parameters"
    [ testCase "dynamic query with aggregation and where condition" $
        assertEqual "" (realStructToContent testSimpleQuery) "SELECT row(truck.id, truck.vehicle_id) FROM truck WHERE truck.id =ANY( '{5a7478bc-4190-44b1-86ce-206f0ca64f43}')"
    ]

testSimpleQuery :: RawSelectStruct Text
testSimpleQuery =
  RawSelectStruct
    { wrapFunctionList = [Row], -- either: "row" or "array_agg", "row"
      selectPart = ["truck.id", "truck.vehicle_id"],
      fromPart = "truck",
      joinList = [],
      whereConditions = renderedTruckFilter,
      groupByList = [],
      havingConditions = Nothing,
      orderByList = [],
      offsetLimit = Nothing
    }
  where
    renderedTruckFilter = filterFieldToMaybeQueryPart (Just "truck") (id truckFilter)

data TruckFilter
  = TruckFilter
      { id :: Maybe (GQLFilterUUID "id")
      }

truckFilter = TruckFilter $ Just $ simpleEqFilter (fromString "5a7478bc-4190-44b1-86ce-206f0ca64f43")

simpleEqFilter maybeUUID = defaultScalarFilter {isIn = (\a -> [a]) <$> maybeUUID}
