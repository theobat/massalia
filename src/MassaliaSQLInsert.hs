{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaSQLInsert
  ( InsertStruct (..),
  )
where

import Data.Maybe (fromMaybe)
import Data.Morpheus.Types.Internal.AST.Selection
  ( Arguments,
    Selection (..),
    SelectionContent (SelectionField, SelectionSet),
    SelectionSet,
    ValidSelection,
    ValidSelectionSet,
  )
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Encoders as Encoders
import MassaliaCore (MassaliaStruct (..))
import Text.Inflections (toUnderscore)

data InsertStruct decoder
  = InsertStruct
      { query :: RawInsertStruct,
        decoder :: Decoders.Composite decoder
      }

data RawInsertStruct
  = RawInsertStruct
      { intoPart :: SelectQueryPart SQLTableName,
        columnList :: [SelectQueryPart SQLColumn],
        valueList :: [[SelectQueryPart SQLValues]],
        returningList :: [SelectQueryPart SQLReturning]
      }

data SQLValues

data SQLReturning

data SQLColumn

data SQLTableName

newtype SelectQueryPart a = SelectQueryPart Snippet deriving (IsString, Semigroup)

test :: [[SelectQueryPart SQLValues]]
test =
  [ ["DEFAULT", "1"]
  ]
