{-# LANGUAGE OverloadedStrings #-}

module SpecStaticInsert
  ( unitTests,
  )
where

import Data.Text (Text)
import MassaliaSchema.Industry.PlantInput (queryTest)
import Test.Tasty
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests =
  testGroup
    "SQL insert queries (no params)"
    [ testCase "static insert values wrapped in select" $
        assertEqual "" "WITH plant_input AS (INSERT INTO \"plant\" (id,name,check_date)\nSELECT * FROM (VALUES  ('00000000-0000-0000-0000-000000000000', 'okokok', '1991-08-22')) as plant_input (id,name,check_date)\nRETURNING *)" (queryTest :: Text)
    ]


