{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module MassaliaSchema.Industry.TruckFilter
  ( TruckFilter (..),
    testInstance
  )
where

import qualified Data.Aeson as JSON
import Data.Data (Data, gmapQ)
import Data.Text (Text)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic, from)
import Massalia.Filter
  ( GQLFilterText,
    GQLFilterUUID,
    GQLScalarFilterCore(isEq, isIn),
    defaultScalarFilter,
  )
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( QueryFormat,
    BinaryQuery,
    TextQuery,
    FromText,
    SQLEncoder,
    MassaliaContext(..)
  )
import Massalia.SQLClass (
    SQLFilter,
    SQLFilterField(filterStruct)
  )
import Data.Morpheus.Types (GQLType(..), directives, typeDirective, Describe (Describe), KIND)
import Protolude

data TruckFilter
  = TruckFilter
      { id :: Maybe GQLFilterUUID,
        vehicleId :: Maybe GQLFilterText
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON, SQLFilter)


instance SQLFilterField TruckFilter where
  filterStruct _ selection value = Nothing

testInstance =
  TruckFilter
    { id = pure $ defaultScalarFilter {isEq = Just nil},
      vehicleId = Nothing
    }

instance GQLType TruckFilter where
  directives _ = typeDirective (Describe "A set of filters for the Truck type")

instance MassaliaContext TruckFilter where
  getDecodeOption = const mempty
  setDecodeOption = const identity