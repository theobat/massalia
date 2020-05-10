{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuantifiedConstraints #-}

module MassaliaSchema.Industry.TruckFilter
  ( TruckFilter (..)
  )
where

import qualified Data.Aeson as JSON
import Data.Data (Data, gmapQ)
import Data.Text (Text)
import Data.UUID (UUID, nil)
import GHC.Generics (Generic, from)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Massalia.Filter
  ( GQLFilterText,
    GQLFilterUUID,
    GQLScalarFilter (isEq, isIn),
    defaultScalarFilter,
    filterFieldToMabeContent,
    filterFieldToMaybeQueryPart,
  )
import qualified Massalia.HasqlDec as Decoders
import Massalia.QueryFormat
  ( HasqlSnippet,
    SQLEncoder
  )
import Massalia.SQLClass (SQLFilter)
import Massalia.SQLPart
  ( AQueryPart,
  )
import Prelude hiding (id)
import qualified Prelude (id)

data TruckFilter
  = TruckFilter
      { id :: Maybe (GQLFilterUUID "id"),
        vehicleId :: Maybe (GQLFilterText "vehicle_id")
      }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON,
    SQLFilter Text, SQLFilter HasqlSnippet)

testInstance =
  TruckFilter
    { id = Just $ defaultScalarFilter {isEq = Just nil},
      vehicleId = Nothing
    }

