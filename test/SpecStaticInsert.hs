{-# LANGUAGE OverloadedStrings #-}

module SpecStaticInsert
  ( unitTests,
  )
where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Massalia.SQLInsert
  ( InsertSchema (InsertSchema),
    ValuesFormatType (PureValues, WrapInSelect),
    valuesToInsert,
    valuesToInsertWrapped,
  )
import Massalia.SQLSelect
import Test.Tasty
import Test.Tasty.HUnit

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

resultExample :: ValuesFormatType -> Text
resultExample insertType = case insertType of
  WrapInSelect -> valuesToInsertWrapped schemaStuff exampleValueList
  PureValues -> valuesToInsert schemaStuff exampleValueList
  where
    schemaStuff :: InsertSchema Text (Text, Int)
    schemaStuff = InsertSchema $ ("example", ["a_text", "an_int"], tupleToQueryFormat)
    exampleValueList = [("okok", 12), ("yeah", 1)]
