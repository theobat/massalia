{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import MassaliaRec
import MassaliaSchema.Industry.Plant (Plant, plantInitSQL)
import MassaliaSchema.Industry.Truck (Truck, truckInitSQL)
import GraphQLMorpheusTestData (truckSelTest, plantSelTest)
import MassaliaSQL (SelectStruct, globalStructureToQuery)
import Data.Text(Text)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

testTruckQuery :: SelectStruct () Truck
testTruckQuery = truckInitSQL truckSelTest

testPlantQuery :: SelectStruct () Plant
testPlantQuery = plantInitSQL plantSelTest

testTruckAcc :: (Text, Truck)
testTruckAcc = truckInitSQL truckSelTest

unitTests = testGroup "Unit tests"
  [ testCase "test simple select query" $
      assertEqual "" (globalStructureToQuery testTruckQuery) "SELECT row(vehicle_id, id) FROM truck "

    , testCase "test simple rec traverse" $
      assertEqual "" (fst testTruckAcc) " "

    , testCase "test nested query" $
        assertEqual "" (globalStructureToQuery testPlantQuery) " "
  -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
  ]