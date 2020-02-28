{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaSQLSelect
  ( SelectStruct (..),
    RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    furtherQualifyWhereJoin,
    structToSubquery,
    ASelectQueryPart(SelectQueryPart)
  )
where

import MassaliaSQLRawSelect (
    RawSelectStruct (..),
    structToContent,
    RowFunction (..),
    defaultAssemblingOptions,
    testAssemblingOptions,
    furtherQualifyWhereJoin,
    structToSubquery,
    ASelectQueryPart(SelectQueryPart),
    addSelectColumns
  )
import qualified Hasql.Decoders as Decoders
import Data.Text(Text)
import qualified Hasql.DynamicStatements.Snippet as Snippet (param)
import qualified Hasql.Encoders as Encoders
import MassaliaSQLPart (IsTextOrString(fromText))
import Text.Inflections (toUnderscore)

data SelectStruct decoder queryFormat
  = SelectStruct
      { query :: RawSelectStruct queryFormat,
        decoder :: Decoders.Composite decoder
      }

getInitialValueSelect :: RawSelectStruct queryFormat -> recordType -> SelectStruct recordType queryFormat
getInitialValueSelect rawStruct defaultRecord = SelectStruct {
  query = rawStruct,
  decoder = pure defaultRecord
}
type Updater a decoder = decoder -> a -> decoder
type ValueDec a = Decoders.Value a

snakeCol :: (IsTextOrString queryFormat) => Text -> Updater a decoder -> ValueDec a -> SelectStruct decoder queryFormat -> SelectStruct decoder queryFormat
snakeCol column = exprCol snakeColumn
  where
    snakeColumn = case toUnderscore column of
        Left _ -> error "Failed at transforming field name to underscore = " <> column
        Right e -> e

exprCol :: (IsTextOrString queryFormat) => Text -> Updater a decoder -> ValueDec a -> SelectStruct decoder queryFormat -> SelectStruct decoder queryFormat
exprCol expr updater hasqlValue selectStruct = selectStruct {
    query = addSelectColumns [fromText expr] (query selectStruct),
    decoder = do
      entity <- decoder selectStruct
      updater entity <$> Decoders.field (Decoders.nonNullable hasqlValue)
  }


-- subColList
-- exprCol :: 

-- simpleCol
-- exprCol
-- subColList  
