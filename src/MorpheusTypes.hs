module MorpheusTypes (
  Arguments,
  ValidArguments,
  Selection (..),
  SelectionContent (SelectionField, SelectionSet),
  SelectionSet,
  ValidSelection,
  ValidSelectionSet,
  ValidSelectionContent,
  Key,
  Position(Position),
  validSelectionField,
  safeFromList,
  Name,
  selectionGen,
  MergeSet(MergeSet)
) where

import Data.Morpheus.Types.Internal.AST.Selection
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
  )
  
import Data.Morpheus.Types.Internal.AST.MergeSet (MergeSet(MergeSet), safeFromList)
import Data.Morpheus.Types.Internal.AST.Base (VALID, Key, Position(Position), Name)

type ValidSelection = Selection VALID
type ValidSelectionSet = SelectionSet VALID
type ValidArguments = Arguments VALID
type ValidSelectionContent = SelectionContent VALID

validSelectionField :: ValidSelectionContent 
validSelectionField = SelectionField

selectionGen :: Name -> ValidArguments -> ValidSelectionContent -> ValidSelection
selectionGen name a content = Selection {
    selectionName = name,
    selectionAlias = Nothing,
    selectionPosition = (Position 0 0),
    selectionArguments = a,
    selectionContent = content
  }

-- makeValidSelectionSet :: [(Key, a)] -> MergeSet a
-- makeValidSelectionSet = safeFromList