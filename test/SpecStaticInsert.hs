{-# LANGUAGE OverloadedStrings #-}

module SpecStaticInsert (unitTests) where

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.String (IsString(fromString))
import MassaliaSQLSelect 
import Test.Tasty
import Test.Tasty.HUnit
import MassaliaSQLInsert
  ( inputToInsertStatement,
    InsertType(WrapInSelect, PureValues)
  )

unitTests =
  testGroup
    "SQL insert queries (no params)"
    [ testCase "static insert values wrapped in select" $
        assertEqual "" "INSERT INTO \"example\" (a_text,an_int)\nSELECT * FROM (VALUES \n('yeah','1'),\n('okok','12')) as values_selection (a_text,an_int)" (resultExample WrapInSelect),
      testCase "static insert pure values" $
        assertEqual "" "INSERT INTO \"example\" (a_text,an_int)\nVALUES \n('yeah','1'),\n('okok','12')" (resultExample PureValues)
    ]

tupleToQueryFormat :: (Text, Int) -> Text
tupleToQueryFormat (aText, anInt) = "'" <> aText <> "','" <> fromString (show anInt) <> "'"

resultExample :: InsertType -> Text
resultExample formatType = inputToInsertStatement (Just formatType) schemaStuff exampleValueList
  where
    schemaStuff :: (Text, [Text], (Text, Int) -> Text)
    schemaStuff = ("example", ["a_text", "an_int"], tupleToQueryFormat)
    exampleValueList = [("okok", 12), ("yeah", 1)]
