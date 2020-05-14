{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Massalia.SelectionTree
where

import Massalia.GenericUtils (GTypeName(gtypename))
import Data.String (String, IsString(fromString))
import Massalia.Utils (simpleSnakeCase)
import Data.Morpheus.Core (SelectionTree)
import qualified Data.Morpheus.Core as MorpheusTree (SelectionTree(..))
import Data.Morpheus.Types (
    Context (Context, currentSelection),
    Resolver, unsafeInternalContext,
  )
import qualified Data.Map as Map
import Protolude

data MassaliaNode = MassaliaNode {
  name :: Text,
  children :: Map Text MassaliaNode
}

leaf :: Text -> MassaliaNode
leaf key = MassaliaNode {
  name = key,
  children = mempty
}

node :: Text -> [Text] -> MassaliaNode
node key child = MassaliaNode {name = key, children= Map.fromList ((\a -> (a, leaf a)) <$> child)}

over :: MassaliaNode -> MassaliaNode -> MassaliaNode
over parent child = parent{children=Map.insert (name child) child $ children parent}

overAll :: MassaliaNode -> [MassaliaNode] -> MassaliaNode
overAll parent child = parent{
    children = Map.fromList $ ((\e -> (name e, e)) <$>) child
  }
nodeOver :: Text -> [MassaliaNode] -> MassaliaNode
nodeOver key childList = leaf key `overAll` childList


fromMorpheusContext :: Context -> MassaliaNode
fromMorpheusContext Context{currentSelection = input} = morpheusNodeToMassaliaNode input

morpheusNodeToMassaliaNode :: (SelectionTree a) => a -> MassaliaNode
morpheusNodeToMassaliaNode input
  | MorpheusTree.isLeaf input = leaf $ MorpheusTree.getName input
  | otherwise = MassaliaNode {
      name = MorpheusTree.getName input,
      children = Map.fromList childrenList 
    }
    where
      childrenList = transformer <$> MorpheusTree.getChildrenList input
      transformer morpheusNode = (MorpheusTree.getName morpheusNode, morpheusNodeToMassaliaNode morpheusNode)

instance MassaliaTree MassaliaNode where
  isLeaf = (Map.null . children)
  getChildrenList = ((snd <$>) . Map.toList . children)
  lookupChildren key val = Map.lookup key (children val)
  foldrChildren foldFunction init n = foldr foldFunction init (children n)
  getName = name
  
class MassaliaTree a where

  -- | leaf test: list of children empty?
  isLeaf :: a -> Bool

  -- | innner node test: @ not . isLeaf @
  isInner :: a -> Bool
  isInner = not . isLeaf
  {-# INLINE isInner #-}

  -- | Get the children
  getChildrenList :: a -> [a]
  
  -- | A loopuk for children. Beware, the complexity will depend on the underlying implementation.
  -- The lookup is done using names
  lookupChildren :: Text -> a -> Maybe a

  -- | Directly foldr on children
  foldrChildren :: (a -> b -> b) -> b -> a -> b

  -- | get a node's name
  getName :: a -> Text

