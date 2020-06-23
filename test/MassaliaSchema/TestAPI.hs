{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module MassaliaSchema.TestAPI where

import GHC.Generics (Generic)
import Massalia.HasqlExec (Pool)
import Massalia.Morpheus (interpreter)
import Massalia.MorpheusTypes
  ( GQLRequest,
    GQLResponse,
    RootResolver (..),
    GQLType,
    Undefined (..),
  )
import MassaliaSchema.Industry.Plant (Plant, plantListQuery)
import MassaliaSchema.Industry.PlantFilter (PlantFilter)
import Massalia.UtilsGQL (Paginated)

api :: Pool -> GQLRequest -> IO GQLResponse
api dbConnectionPool = interpreter $ rootResolver (Just dbConnectionPool)

apiWithoutDB :: GQLRequest -> IO GQLResponse
apiWithoutDB = interpreter $ rootResolver Nothing

rootResolver :: Maybe Pool -> RootResolver IO () Query Undefined Undefined
rootResolver dbConnectionPool =
  RootResolver
    { queryResolver = rootQuery dbConnectionPool,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

-- data Mutation m
--   = Mutation
--       { plantListInsert :: PlantInputDBContext -> m [Plant]
--       }
--   deriving (Generic, GQLType)

data Query m
  = Query
      { plantListPaginated :: Paginated PlantFilter -> m [Plant]
      }
  deriving (Generic, GQLType)

rootQuery :: (Maybe Pool) -> Query (_ _ () IO)
rootQuery dbConnectionPool =
  Query
    { plantListPaginated = plantListQuery dbConnectionPool
    }
