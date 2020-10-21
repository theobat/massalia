{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Massalia.SQLUtils where

import Protolude
import Massalia.QueryFormat (inParens)

rowsAssembler ::
  (Foldable collection, Semigroup queryFormat, IsString queryFormat) =>
  queryFormat -> collection queryFormat -> queryFormat
rowsAssembler sep input = assembledRows
  where
    rowSeparator a (0, previousRows) = rowSeparatorGeneric sep a (0, previousRows)
    rowSeparator a (index, previousRows) = rowSeparatorGeneric ("," <> sep) a (index, previousRows)
    rowSeparatorGeneric isep a (index, previousRows) = (index + 1, previousRows <> isep <> a)
    (_, assembledRows) = foldr rowSeparator (0, "") input

insertIntoWrapper ::
  (Monoid queryFormat, IsString queryFormat) => queryFormat -> queryFormat -> queryFormat
insertIntoWrapper tableName assembledColList = "INSERT INTO \"" <> tableName <> "\" " <> assembledColList

selectWrapper ::
  (Monoid queryFormat, IsString queryFormat) => queryFormat -> queryFormat -> queryFormat -> queryFormat
selectWrapper name assembledColList valueRows = "SELECT * FROM (" <> valueRows <> ") as " <> name <> " " <> assembledColList

data NodeUtils = NodeUtils {
  tableNames :: Map Text Text
}

data SQLWith queryFormat = SQLWith {
  withName :: queryFormat,
  withBody :: queryFormat
} deriving (Show, Eq)

-- | Inlines a struct of WITH query parts:
--
-- Example:
--
-- >>> inlineWith Nothing []
-- "WITH "
-- >>> inlineWith Nothing [SQLWith "test" "select 1"]
-- "WITH \"test\" AS (select 1)"
inlineWith :: (Foldable collection, Semigroup queryFormat, IsString queryFormat) =>
  -- | potential future options 
  Maybe () ->
  collection (SQLWith queryFormat) ->
  queryFormat
inlineWith _ input = foldr' assembler "WITH " input
  where
    assembler (SQLWith nameV bodyV) existing = existing <> ("\"" <> nameV <> "\"" <> " AS " <> (inParens bodyV))

