{- |
   Module     : Massalia.TreeClass
   Copyright  : Copyright (C) 2005 Uwe Schmidt
   License    : MIT

   Maintainer : Théophile Batoz (theophile.batoz@kanso.ink)
   Stability  : experimental
   Portability: portable

   Interface definition for trees, mostly copy pasted from "Data.Tree.Class" in hxt package.
   
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Massalia.TreeClass

where

import Massalia.QueryFormat
  ( HasqlSnippet
  )
import Massalia.GenericUtils (GTypeName(gtypename))
import Data.String (String, IsString(fromString))
import GHC.Generics (
    U1,
    D,
    M1(M1),
    datatypeName
  )
import Massalia.Utils (simpleSnakeCase)
import Massalia.MorpheusTypes
  ( Key,
    Selection (Selection, selectionContent, selectionName),
    SelectionContent (..),
    ValidSelection,
    ValidSelectionSet,
    validSelectionToSelectionSet,
  )
import Massalia.MorpheusTypes
  ( Context (Context, currentSelection),
    GQLRootResolver (..),
    GQLType,
    unsafeInternalContext,
  )
import Protolude

-- ------------------------------------------------------------

instance Tree ValidSelection where
  isLeaf node = case selectionContent node of
    SelectionField -> True
    _ -> False
  getName = selectionName
  getChildren node = case selectionContent node of
    SelectionField -> mempty
    (SelectionSet deeperSel) -> toList deeperSel
  foldrChildren iterator init node = case selectionContent node of
    SelectionField -> init
    (SelectionSet deeperSel) -> foldr iterator init deeperSel

-- | The interface for trees

class Tree a where

  -- | leaf test: list of children empty?
  isLeaf :: a -> Bool

  -- | innner node test: @ not . isLeaf @
  isInner :: a -> Bool
  isInner = not . isLeaf
  {-# INLINE isInner #-}

  -- | Get the children
  getChildren :: a -> [a]

  -- | Directly foldr on children
  foldrChildren :: (a -> b -> b) -> b -> a -> b

  -- | get a node's name
  getName :: a -> Text
