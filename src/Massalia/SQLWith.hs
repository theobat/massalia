{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Massalia.SQLWith
  ( withXAs,
    xAs
  )
where

import GHC.Generics (
    U1,
    D,
    M1(M1),
    datatypeName
  )
import Protolude

-- | A simple @WITH@ prefix on 'xAs' result.
-- The parameters are the same as 'xAs' so the doc is not reproduced here
--  and the parameters have dumb name (a b c d)
withXAs ::
  forall queryFormat collection underlyingType dbContextType.
  (IsString queryFormat, Monoid queryFormat, Foldable collection) =>
  queryFormat ->
  (collection underlyingType -> queryFormat) ->
  (dbContextType -> collection underlyingType) ->
  dbContextType ->
  queryFormat
withXAs a b c d = "WITH " <> xAs a b c d

-- | A function to create a CTE instance from a collection of haskell records and a 
-- function to turn this record into query format.
xAs ::
  (IsString queryFormat, Monoid queryFormat, Foldable collection) =>
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
xAs tableAlias toQueryFormat toTypeCollection input = result
  where
    result = headerRes
    headerRes = "\"" <> tableAlias <> "\" as " <> columnListAssembled
    columnListAssembled = "(" <> innerQuery <> ")"
    innerQuery = toQueryFormat (toTypeCollection input)
