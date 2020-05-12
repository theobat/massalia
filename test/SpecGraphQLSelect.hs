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
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import GraphQLMorpheusTestData (plantQuery, truckQuery)
import Massalia.Filter (GQLFilterText, GQLScalarFilter (isEq), defaultScalarFilter)
import qualified Massalia.HasqlDec as Decoders
import Massalia.HasqlExec (dynamicallyParameterizedStatement)
import Massalia.SQLSelect (RawSelectStruct (..), RowFunction (ArrayAgg, Row), SelectStruct (SelectStruct), selectStructToContent, testAssemblingOptions)
import MassaliaSchema.Industry.Plant (Plant, defaultFilter, plantInitSQL)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import qualified SpecDynamicSelect
import qualified SpecStaticSelect
import Test.Tasty
import Test.Tasty.HUnit

-- testTruckQuery :: SelectStruct () Truck
-- testTruckQuery = truckInitSQL truckSelection

testPlantQuery :: SelectStruct Plant Text
testPlantQuery = plantInitSQL defaultFilter plantQuery

testTruckList :: SelectStruct Truck Text
testTruckList = truckInitSQL Nothing truckQuery

unitTests =
  testGroup
    "Test graphql AST to SQL query"
    [ testCase "test simple Truck query" $
        assertEqual "" (selectStructToContent testAssemblingOptions testTruckList) "SELECT row(truck.id) FROM truck",
      testCase "test simple Plant->Truck query" $
        assertEqual "" (selectStructToContent testAssemblingOptions testPlantQuery) "SELECT row((SELECT array_agg(row(truck.id)) FROM truck JOIN truck_plant ON truck.id=truck_plant.truck_id WHERE truck_plant.plant_id=plant.id GROUP BY plant.id), plant.id) FROM plant LIMIT 20"
    ]
