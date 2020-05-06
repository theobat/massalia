{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Massalia.SQLClass
-- Description : A module to define SQL-related typeclasses
-- with generic deriving capabilities
module Massalia.SQLClass
  ( SQLName(..),
    SQLColumns(..),
    SQLValues(..),
    DBContext(..)
  )
where

import Massalia.QueryFormat
  (
    FromText,
    SQLEncoder(sqlEncode),
    HasqlSnippet,
    DefaultParamEncoder,
    param,
    (§),
    inParens
  )
import Massalia.SQLUtils (selectWrapper, rowsAssembler)
import Massalia.GenericUtils (GTypeName(gtypename), GSelectors(selectors))
import Data.String (String, IsString(fromString))
import GHC.Generics (
    U1,
    D,
    S,
    C,
    R,
    M1(M1),
    datatypeName,
    K1(K1)
  )
import Massalia.Utils (simpleSnakeCase, intercalate)
import Protolude hiding (intercalate)

-- | This class represents all the haskell types with a corresponding SQL
-- name whether this name is the group alias in a @WITH@ CTE query or an 
-- ordinary table name.
-- e.g.
-- @
--     sqlName $ HaskellRecord { name = "some text" }
-- @
-- Would default-yield something like @"haskell_record"@.
-- 
class SQLName a where
  sqlName :: (IsString queryFormat) => queryFormat
  -- | The default SQLName is merely a snake case alternative of the 
  default sqlName :: (IsString queryFormat, Generic a, GTypeName (Rep a)) => queryFormat
  sqlName = fromString snakeTypename
    where
      snakeTypename = simpleSnakeCase typename
      typename = gtypename @(Rep a)

-- | This class represents all the haskell types with a corresponding list
-- of SQL column associated (mainly haskell records, representing sql tables).
-- e.g.
-- @
--     sqlColumns $ HaskellRecord { name = "some text", identifier = 1234, pID = "really ?"}
-- @
-- Would yield something like @["name", "identifier", "p_id"]@.
-- 
class SQLColumns a where
  sqlColumns :: (IsString queryFormat) => [queryFormat]
  default sqlColumns :: (IsString queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
  sqlColumns = snakeTypename
    where
      snakeTypename = (fromString . simpleSnakeCase . fst) <$> list
      list = selectors @(Rep a)

-- | This class represents all the haskell types with a corresponding 'toSQLValues'
-- function. It's a function meant to encode a haskell record as an SQL comma separated
-- list of parametrized values.
-- e.g.
-- @
--     toSQLValues $ HaskellRecord { name = "some text", identifier = 1234, pID = "really ?"}
-- @
-- Would yield something like @(?, ?, ?)@ with @["some text", 1234, "really ?"]@ parameters.
-- Or @ ("some text", 1234, "really ?") @ if the 'queryFormat' is 'Text'.
-- 
class SQLValues queryFormat a where
  toSQLValues :: a -> [queryFormat]
  default toSQLValues :: (Generic a, GValues (Rep a) queryFormat) => a -> [queryFormat]
  toSQLValues value = goToValues (from value)

class GValues f queryFormat where
  goToValues :: f a -> [queryFormat]

instance (Monoid queryFormat) => GValues U1 queryFormat where
  goToValues U1 = mempty
instance (GValues a queryFormat, GValues b queryFormat) => GValues (a :*: b) queryFormat where
  goToValues (a :*: b) = goToValues a <> goToValues b

instance (GValues a queryFormat) => GValues (M1 D c a) queryFormat where
  goToValues (M1 x) = goToValues x
instance (GValues a queryFormat) => GValues (M1 S c a) queryFormat where
  goToValues (M1 x) = goToValues x
instance (GValues a queryFormat) => GValues (M1 C c a) queryFormat where
  goToValues (M1 x) = goToValues x

instance (SQLEncoder a Text, GValues (K1 i a) Text) =>
  GValues (K1 i a) Text where
  goToValues (K1 val) = [(sqlEncode val)]
instance (SQLEncoder a HasqlSnippet, GValues (K1 i a) HasqlSnippet) =>
  GValues (K1 i a) HasqlSnippet where
  goToValues (K1 val) = [(sqlEncode val)]

---------------------------- DBContext queries

class DBContext queryFormat record where
  toWithQuery :: () -> record -> queryFormat
  default toWithQuery :: (
      IsString queryFormat, Monoid queryFormat, Generic record,
      GDBContext (Rep record) queryFormat
    ) =>
    () -> record -> queryFormat
  toWithQuery options value = "WITH " <> (intercalate "," genericRes)
    where genericRes = gtoWithQuery options (from value)

class GDBContext f queryFormat where
  gtoWithQuery :: () -> f a -> [queryFormat]

instance (Monoid queryFormat) => GDBContext U1 queryFormat where
  gtoWithQuery _ U1 = mempty
instance (Monoid queryFormat, GDBContext a queryFormat, GDBContext b queryFormat) =>
  GDBContext (a :*: b) queryFormat where
  gtoWithQuery options (a :*: b) = withStatement a <> withStatement b
    where
      withStatement a = gtoWithQuery options a

instance (GDBContext a queryFormat) => GDBContext (M1 D c a) queryFormat where
  gtoWithQuery options (M1 x) = gtoWithQuery options x
instance (GDBContext a queryFormat) => GDBContext (M1 S c a) queryFormat where
  gtoWithQuery options (M1 x) = gtoWithQuery options x
instance (GDBContext a queryFormat) => GDBContext (M1 C c a) queryFormat where
  gtoWithQuery options (M1 x) = gtoWithQuery options x

instance (
    Foldable collection,
    Functor collection,
    Monoid queryFormat,
    IsString queryFormat,
    SQLName a,
    SQLColumns a,
    SQLValues queryFormat a
  ) =>
  GDBContext (K1 i (collection a)) queryFormat where
  gtoWithQuery _ (K1 val) = pure $ sqlName @a <> " AS " <> (inParens selectInstance)
    where selectInstance = selectValuesQuery () val
    -- where values = 
-- instance (SQLEncoder a HasqlSnippet, GValues (K1 i a) HasqlSnippet) =>
--   GValues (K1 i a) HasqlSnippet where
--   goToValues (K1 val) = [(sqlEncode val)]


selectValuesQuery ::
  forall collection recordType queryFormat. (
    Foldable collection,
    Functor collection,
    IsString queryFormat,
    Monoid queryFormat,
    SQLValues queryFormat recordType,
    SQLColumns recordType,
    SQLName recordType
  ) =>
  () -> collection recordType -> queryFormat
selectValuesQuery _ recordCollection = result
  where
    result = selectWrapper name cols assembledRows
    name = sqlName @recordType
    assembledRows = ("VALUES " <>) $ rowsAssembler " " listOfRows
    listOfRows = (inParens . intercalate ",") <$> listOfListOfValues
    listOfListOfValues = toSQLValues @queryFormat <$> recordCollection
    cols = inParens $ intercalate "," $ sqlColumns @recordType