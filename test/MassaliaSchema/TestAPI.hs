{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MassaliaSchema.TestAPI where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (GQLRequest, GQLResponse, GQLRootResolver (..), GQLType, IORes, QUERY, ResolveQ, Resolver, Undefined (..))
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import MassaliaSchema.Industry.DBContext (PlantInputDBContext)
import MassaliaSchema.Industry.Plant (Plant, PlantListQueryFilter, plantListQuery)
import MassaliaSchema.Industry.Truck (Truck)
import MorpheusTypes

api :: Connection -> GQLRequest -> IO GQLResponse
api dbConnection = interpreter $ rootResolver dbConnection

rootResolver :: Connection -> GQLRootResolver IO () Query Undefined Undefined
rootResolver dbConnection =
  GQLRootResolver
    { queryResolver = rootQuery dbConnection,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

data Mutation m
  = Mutation
      { plantListInsert :: PlantInputDBContext -> m [Plant]
      }
  deriving (Generic, GQLType)

data Query m
  = Query
      { plantListPaginated :: PlantListQueryFilter -> m [Plant]
      }
  deriving (Generic, GQLType)

rootQuery :: Connection -> Query (Resolver QUERY () IO)
rootQuery dbConnection =
  Query
    { plantListPaginated = plantListQuery dbConnection
    }
