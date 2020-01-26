{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaCore
    (
    MassaliaStruct(..)
    ) where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Data.Text (Text)
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( ValidSelectionSet, ValidSelection,
                                                SelectionSet, Selection(..), Arguments,
                                                SelectionContent(SelectionField, SelectionSet))
                                                
type ReUpdater recordType fieldType = (recordType -> fieldType -> recordType)
type ReEUpdater recordType fieldType wrapper = (recordType -> wrapper fieldType -> recordType)
type ReGetter recordType fieldType = (recordType -> fieldType)
  
class MassaliaStruct wrapper someType recordType where
  simpleCol :: Show fieldType => Text -> ReUpdater recordType fieldType -> ReGetter recordType fieldType -> Decoders.Value fieldType -> wrapper someType recordType -> wrapper someType recordType
  subColWrap :: (ValidSelectionSet -> wrapper someType nestedRecordType) -> (Text, Text) -> ReEUpdater recordType nestedRecordType []
    -> ValidSelection -> wrapper someType recordType -> wrapper someType recordType

