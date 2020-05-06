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
        assertEqual "" "INSERT INTO \"example\" (a_text,an_int)\nSELECT * FROM (VALUES \n('yeah','1'),\n('okok','12')) as values_selection (a_text,an_int)" (queryTest)
    ]


