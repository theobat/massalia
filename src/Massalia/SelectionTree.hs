{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Massalia.SelectionTree
where

import qualified Data.Morpheus.Core as MorpheusTree (SelectionTree(..))
import Data.Morpheus.Types (
    ResolverContext (ResolverContext, currentSelection),
  )
import qualified Data.Map.Strict as Map
import Data.Morpheus.Types.Internal.AST (Selection, VALID, FieldName, unpackName)
import Protolude
import Data.Aeson (ToJSON, Value (String, Array), FromJSON (parseJSON))
import Data.Aeson.Types (ToJSON(toJSON))
import Data.Vector ((!), tail, map, Vector, (!?))
import Control.Monad (MonadFail(fail))
import Data.Set (fromList, insert)

-- | A tree structure but with indexed-by-name children
data MassaliaNode = MassaliaNode {
  name :: !Text,
  children :: (Map Text MassaliaNode)
} deriving (Eq, Show)

-- | left associative (prefer data from the left side in case of conflict).
instance Semigroup MassaliaNode where
  (<>) a b = MassaliaNode {
      name=name a,
      children = children a <> children b
    }



leaf :: Text -> MassaliaNode
leaf !key = MassaliaNode {
  name = key,
  children = mempty
}

node :: Text -> [Text] -> MassaliaNode
node !key !child = MassaliaNode {name = key, children= Map.fromList ((\a -> (a, leaf a)) <$> child)}

over :: MassaliaNode -> MassaliaNode -> MassaliaNode
over parent child = parent{children=Map.insert (name child) child $ children parent}

overAll :: MassaliaNode -> [MassaliaNode] -> MassaliaNode
overAll parent child = parent{
    children = Map.fromList $ ((\e -> (name e, e)) <$>) child
  }
nodeOver :: Text -> [MassaliaNode] -> MassaliaNode
nodeOver key childList = leaf key `overAll` childList

fromMorpheusContextM ::  MonadReader ResolverContext m => m MassaliaNode
fromMorpheusContextM = asks fromMorpheusContext

fromMorpheusContext :: ResolverContext -> MassaliaNode
fromMorpheusContext ResolverContext{currentSelection = input} = morpheusNodeToMassaliaNode input

{-# INLINABLE morpheusNodeToMassaliaNode #-}
morpheusNodeToMassaliaNode :: Selection VALID -> MassaliaNode
morpheusNodeToMassaliaNode input
  | MorpheusTree.isLeaf input = leaf $ textName
  | otherwise = MassaliaNode {
      name = textName,
      children = Map.fromList childrenList
    }
    where
      textName = getTextName input
      getTextName = readName . MorpheusTree.getName
      childrenList = transformer <$> MorpheusTree.getChildrenList input
      transformer morpheusNode = (readName $ MorpheusTree.getName morpheusNode, morpheusNodeToMassaliaNode morpheusNode)

instance MassaliaTree MassaliaNode where
  {-# INLINE isLeaf #-}
  isLeaf = Map.null . children
  getChildrenList = (snd <$>) . Map.toList . children
  {-# INLINE lookupChildren #-}
  lookupChildren !key !val = Map.lookup key (children val)
  {-# INLINE foldrChildren #-}
  foldrChildren foldFunction init n = foldr' foldFunction init (children n)
  {-# INLINE getName #-}
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

readName :: FieldName -> Text
readName = unpackName

data JsonMassaliaTree = JsonMassaliaNode Text (Set JsonMassaliaTree) | JsonMassaliaLeaf Text
  deriving (Eq, Show, Generic)

instance MassaliaTree JsonMassaliaTree where
  isLeaf (JsonMassaliaNode _ _) = False
  isLeaf (JsonMassaliaLeaf _) = True

  getChildrenList (JsonMassaliaLeaf _) = []
  getChildrenList (JsonMassaliaNode _ children) = toList children

  lookupChildren key n = find ((== key) . getName) (getChildrenList n)

  foldrChildren f init n = foldr' f init (getChildrenList n)

  getName (JsonMassaliaLeaf name) = name
  getName (JsonMassaliaNode name _) = name

instance Ord JsonMassaliaTree where
  compare = compare `on` getName
instance FromJSON JsonMassaliaTree where
  parseJSON (String name) = pure $ JsonMassaliaLeaf name
  parseJSON (Array arr) = do
    name <- maybe (fail "No name in nested node") parseJSON (arr !? 0)
    childrenVector :: Vector JsonMassaliaTree <- mapM parseJSON (tail arr)
    let childrenSet = foldr' insert mempty childrenVector
    return $ JsonMassaliaNode name childrenSet
  parseJSON _ = fail "Invalid JSON Tree (Expected leaf string or array)"
instance ToJSON JsonMassaliaTree where
  toJSON (JsonMassaliaLeaf name) = toJSON name
  toJSON (JsonMassaliaNode name children) = toJSON (JsonMassaliaLeaf name : toList children)