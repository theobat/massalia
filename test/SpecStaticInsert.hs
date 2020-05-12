{-# LANGUAGE OverloadedStrings #-}

module SpecStaticInsert
  ( unitTests,
  )
where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import MassaliaSchema.Industry.PlantInput (queryTest)
import Massalia.SQLSelect
import Test.Tasty
import Test.Tasty.HUnit

unitTests =
  testGroup
    "SQL insert queries (no params)"
    [ testCase "static insert values wrapped in select" $
        assertEqual "" "WITH plant_input AS (SELECT * FROM (VALUES  ('00000000-0000-0000-0000-000000000000','okokok')) as plant_input (id,name))" (queryTest :: Text)
    ]


