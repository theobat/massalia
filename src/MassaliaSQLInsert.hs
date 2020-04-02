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
import MorpheusTypes
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
import Text.Inflections (toUnderscore)
import MassaliaSQLPart (
    AQueryPart(AQueryPartConst),
    getContent,
    getListContent,
    getMaybeContent,
    AssemblingOptions(..),
    defaultAssemblingOptions,
    testAssemblingOptions
  )

data InsertStruct decoder content
  = InsertStruct
      { query :: RawInsertStruct content,
        decoder :: Decoders.Composite decoder
      }

data RawInsertStruct content
  = RawInsertStruct
      { intoPart :: AQueryPart SQLTableName content,
        columnList :: [AQueryPart SQLColumn content],
        valueList :: [[AQueryPart SQLValues content]],
        returningList :: [AQueryPart SQLReturning content]
      }

-- | A type to represent SQL values used in canonical insert statements such as
-- insert into dummy (..) values (this is what we are talking about)
data SQLValues

-- | A type to represent SQL values returned after insertion
-- insert into dummy (..) values (..) returning this, is, what, we ,are, talking, about
data SQLReturning

data SQLColumn

data SQLTableName

test :: [[AQueryPart SQLValues String]]
test =
  [ ["DEFAULT", "1"]
  ]
