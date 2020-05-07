{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MassaliaSchema.TestAPI where

import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics (Generic)
import Massalia.HasqlExec (Pool)
import Massalia.Morpheus (interpreter)
import Massalia.MorpheusTypes
  ( GQLRequest,
    GQLResponse,
    GQLRootResolver (..),
    GQLType,
    IORes,
    QUERY,
    ResolveQ,
    Resolver,
    Undefined (..),
  )
import MassaliaSchema.Industry.DBContext (PlantInputDBContext)
import MassaliaSchema.Industry.Plant (Plant, PlantListQueryFilter, plantListQuery)
import MassaliaSchema.Industry.Truck (Truck)

api :: Pool -> GQLRequest -> IO GQLResponse
api dbConnectionPool = interpreter $ rootResolver dbConnectionPool

rootResolver :: Pool -> GQLRootResolver IO () Query Undefined Undefined
rootResolver dbConnectionPool =
  GQLRootResolver
    { queryResolver = rootQuery dbConnectionPool,
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

rootQuery :: Pool -> Query (Resolver QUERY () IO)
rootQuery dbConnectionPool =
  Query
    { plantListPaginated = plantListQuery dbConnectionPool
    }
