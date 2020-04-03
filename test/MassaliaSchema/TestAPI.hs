{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.TestAPI where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (GQLRequest,GQLResponse, GQLRootResolver (..), GQLType, IORes, Undefined (..), ResolveQ, Resolver, QUERY)
import MorpheusTypes
import MassaliaSchema.Industry.Plant (Plant)
import MassaliaSchema.Industry.Truck (Truck)
import GHC.Generics (Generic)
import MassaliaSchema.Industry.Plant (plantListQuery, PlantListQueryFilter)

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = rootQuery,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

data Query m
  = Query
      { plantListPaginated :: PlantListQueryFilter ->  m [Plant]
      }
  deriving (Generic, GQLType)

rootQuery :: Query (Resolver QUERY () IO)
rootQuery = Query {
  plantListPaginated = plantListQuery
}

  