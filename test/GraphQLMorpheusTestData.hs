{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
    truckSelection,
  )
where

import Data.Morpheus.Kind (ENUM, OBJECT, SCALAR)
import Data.Morpheus.Types
  ( GQLRequest (..),
    GQLResponse,
    GQLRootResolver (..),
    GQLScalar (..),
    GQLType (..),
    IORes,
    ResolveQ,
    Resolver (..),
    ScalarValue (String),
    Undefined (..),
    liftEither,
  )
import Massalia.MorpheusTypes
  ( Arguments,
    Key,
    MergeSet (MergeSet),
    Name,
    Position (Position),
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
    ValidArguments,
    ValidSelection,
    ValidSelectionContent,
    ValidSelectionSet,
    selectionGen,
    validSelectionField,
  )

norAgs = undefined

organizationQuery = selectionGen "organizationList" norAgs organizationContent

organizationContent = SelectionSet organizationSelection

organizationSelection =
  MergeSet
    [ selectionGen "id" norAgs validSelectionField,
      plantQuery
    ]

-- plantSelection :: ValidSelectionSet
-- plantSelection = undefined
plantQuery = selectionGen "plantList" norAgs plantContent

plantContent = SelectionSet plantSelection

plantSelection =
  MergeSet
    [ selectionGen "id" norAgs validSelectionField,
      truckQuery
    ]

truckQuery = selectionGen "truckList" norAgs truckContent

truckContent = SelectionSet truckSelection

truckSelection =
  MergeSet
    [ selectionGen "id" norAgs validSelectionField,
      selectionGen "vehicleId" norAgs validSelectionField
    ]

testSelection = organizationSelection
