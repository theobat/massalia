{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MassaliaSQLWith (withXAs) where
import Protolude
import MassaliaQueryFormat
  ( HasqlSnippet,
    QueryFormat (fromText, param),
  )

-- | A function to create insert statements out of haskell records.
--
withXAs ::
  (QueryFormat queryFormat, Foldable collection) =>
  -- | First is the with group alias.
  queryFormat ->
  -- | The query format transformer
  (collection underlyingType -> queryFormat) ->
  -- | The query format transformer
  (dbContextType -> collection underlyingType) ->
  -- | A collection of literal values to encode into the query.
  dbContextType ->
  -- | The end result for the insert query.
  queryFormat
withXAs tableAlias toQueryFormat toTypeCollection input = result 
  where
    result = headerRes
    headerRes = "WITH \"" <> tableAlias <> "\" as " <> columnListAssembled
    columnListAssembled = "(" <> innerQuery <> ")"
    innerQuery = toQueryFormat (toTypeCollection input)
