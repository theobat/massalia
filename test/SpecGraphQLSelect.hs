{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SpecGraphQLSelect (unitTests) where

import Data.Text (Text)
import GraphQLMorpheusTestData (plantSelTest, truckSelTest)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import qualified Hasql.Session as Session
import GHC.Generics (Generic)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import MassaliaFilter (GQLFilterText, defaultScalarFilter, GQLScalarFilter(isEq))

import MassaliaSQLSelect (SelectStruct(SelectStruct), RawSelectStruct(..), RowFunction(ArrayAgg, Row), selectStructToContent, testAssemblingOptions)
import MassaliaSchema.Industry.Plant (Plant, plantInitSQL)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import Test.Tasty
import Test.Tasty.HUnit
import GHC.TypeLits (Symbol)
import qualified SpecStaticSelect
import qualified SpecDynamicSelect


-- testTruckQuery :: SelectStruct () Truck
-- testTruckQuery = truckInitSQL truckSelTest

testPlantQuery :: SelectStruct Plant String
testPlantQuery = plantInitSQL () plantSelTest

testTruckList :: SelectStruct Truck String
testTruckList = truckInitSQL () truckSelTest

  

unitTests =
  testGroup
    "Test graphql AST to SQL query"
    [ testCase "test simple Truck query" $
        assertEqual "" (selectStructToContent testAssemblingOptions testTruckList) "SELECT row(id) FROM truck",
      testCase "test simple Plant->Truck query" $
        assertEqual "" (selectStructToContent testAssemblingOptions testPlantQuery) "SELECT row((SELECT array_agg(row(id)) FROM truck JOIN truck_plant ON truck.id=truck_plant.truck_id WHERE truck_plant.plant_id=plant.id GROUP BY plant.id), id) FROM plant"
    ]

