{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SpecAPI (unitTests) where

import Data.Text (Text)
import MassaliaSQLSelect (
    RawSelectStruct (..), structToContent, RowFunction(ArrayAgg, Row), testAssemblingOptions,
    structToSubquery, AssemblingOptions
  )
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (id)
import MassaliaFilter (GQLFilterUUID, defaultScalarFilter, GQLScalarFilter(isIn), filterFieldToMaybeQueryPart)
import Data.UUID
import MassaliaSchema.Industry.Plant (plantListQuery)
import MassaliaSchema.TestAPI (api)
import qualified Hasql.Connection as Connection
import Data.Morpheus.Types (GQLRequest (..))

unitTests =
  testGroup
    "GQL API plantList Simple"
    [ testCase "dummy query" $
        assertEqual "" "" ""
    ]
  
