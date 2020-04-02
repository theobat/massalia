{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GraphQLMorpheusTestData
    ( testSelection,
      organizationQuery,
      organizationContent,
      organizationSelection,
      plantQuery,
      plantContent,
      plantSelection,
      truckQuery,
      truckContent,
      truckSelection
    ) where

import           Data.Morpheus.Kind     (OBJECT, ENUM, SCALAR)
import           Data.Morpheus.Types        (ScalarValue(String), GQLScalar(..), ResolveQ, liftEither, GQLRequest(..),
  GQLResponse, Resolver (..), IORes, GQLRootResolver (..), GQLType(..), Undefined (..))
import MorpheusTypes
                                                (
                                                  ValidArguments,
                                                SelectionContent(SelectionField, SelectionSet),
                                                ValidSelectionContent,
                                                ValidSelectionSet,
                                                ValidSelection,
                                                SelectionSet,
                                                Selection(..),
                                                Key, Position(Position),
                                                Name,
                                                Arguments,
                                                validSelectionField,
                                                safeFromList,
                                                selectionGen,
                                                MergeSet(MergeSet)
                                                )



norAgs = undefined

organizationQuery = selectionGen "organizationList" norAgs organizationContent
organizationContent = SelectionSet organizationSelection
organizationSelection = MergeSet
  [ selectionGen "id" norAgs validSelectionField
  , plantQuery
  ]

-- plantSelection :: ValidSelectionSet
-- plantSelection = undefined
plantQuery = selectionGen "plantList" norAgs plantContent
plantContent = SelectionSet plantSelection
plantSelection = MergeSet
  [ selectionGen "id" norAgs validSelectionField
  , truckQuery
  ]

truckQuery = selectionGen "truckList" norAgs truckContent
truckContent = SelectionSet truckSelection
truckSelection = MergeSet [
  selectionGen "id" norAgs validSelectionField
  , selectionGen "vehicleId" norAgs validSelectionField
  ]

testSelection = organizationSelection
