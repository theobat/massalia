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
    organizationSelection,
    plantQuery,
    plantSelection,
    truckQuery,
    truckSelection,
  )
where

import Massalia.SelectionTree (MassaliaNode, leaf, over, overAll, node, nodeOver)

-- norAgs = undefined

-- organizationQuery = selectionGen "organizationList" norAgs organizationContent

-- organizationContent = SelectionSet organizationSelection

-- organizationSelection =
--   MergeSet
--     [ selectionGen "id" norAgs validSelectionField,
--       plantQuery
--     ]
organizationQuery = node2 "organizationList" organizationSelection [
    "plantList" `node` plantSelection
  ]
organizationSelection = ["id"]

plantQuery = node2 "plantList" plantSelection [
    "truckList" `node` truckSelection
  ]
plantSelection = ["id"]
truckQuery = "truckList" `node` truckSelection
truckSelection = ["id"]

node2 name leafList nodeList = nodeOver name ((leaf <$> leafList) <> nodeList)

testSelection = plantQuery