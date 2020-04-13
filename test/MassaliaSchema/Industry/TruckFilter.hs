{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module MassaliaSchema.Industry.TruckFilter (
    TruckFilter(..),
    toQueryPart
) where
import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID, nil)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as Decoders
import qualified Data.Aeson as JSON
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import MassaliaQueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
  )
import MassaliaSQLPart (
    AQueryPart
  )
import MassaliaFilter (
  GQLFilterUUID,
  GQLFilterText,
  defaultScalarFilter,
  GQLScalarFilter(isEq, isIn),
  filterFieldToMaybeQueryPart,
  filterFieldToMabeContent
  )
import Data.Data (Data, gmapQ)

data TruckFilter = TruckFilter {
  id :: Maybe (GQLFilterUUID "id"),
  vehicleId :: Maybe (GQLFilterText "vehicle_id")
} deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON, Data)


testInstance = TruckFilter {
  id = Just $ defaultScalarFilter { isEq = Just nil },
  vehicleId = Nothing
}

toQueryPart :: (QueryFormat content) => Maybe TruckFilter -> Maybe (MassaliaSQLPart.AQueryPart partType content)
toQueryPart Nothing = mempty
toQueryPart (Just TruckFilter{
    id = idVal,
    vehicleId = vehicleIdVal
  }) = (
      filterFieldToMaybeQueryPart tableName idVal <>
      filterFieldToMaybeQueryPart tableName vehicleIdVal
    )
  where tableName = Just "truck"

  