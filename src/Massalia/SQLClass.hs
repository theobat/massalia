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

-- |
-- Module      : Massalia.SQLClass
-- Description : A module to define SQL-related typeclasses
-- with generic deriving capabilities
module Massalia.SQLClass
  ( SQLName(..),
    SQLColumns(..),
    SQLValues(..)
  )
where

import Massalia.QueryFormat
  ( TextEncoder,
    DefaultParamEncoder,
    HasqlSnippet,
    QueryFormat (fromText, param),
    (§)
  )
import Massalia.GenericUtils (GTypeName(gtypename), GSelectors(selectors))
import Data.String (String, IsString(fromString))
import GHC.Generics (
    U1,
    D,
    M1(M1),
    datatypeName,
    K1(K1)
  )
import Massalia.Utils (simpleSnakeCase)
import Protolude

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
  sqlName :: (QueryFormat queryFormat) => queryFormat
  -- | The default SQLName is merely a snake case alternative of the 
  default sqlName :: (Generic a, GTypeName (Rep a), QueryFormat queryFormat) => queryFormat
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
  sqlColumns :: (QueryFormat queryFormat) => [queryFormat]
  default sqlColumns :: (Generic a, GSelectors (Rep a), QueryFormat queryFormat) => [queryFormat]
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
class SQLValues a where
  toSQLValues :: (QueryFormat queryFormat) => a -> [queryFormat]
  default toSQLValues :: (Generic a, GSQLValues (Rep a), QueryFormat queryFormat, QueryFormat [queryFormat]) => a -> [queryFormat]
  toSQLValues value = gtoSQLValues (from value)
  parametrizedField :: Bool
  parametrizedField = True

class GSQLValues f where
  gtoSQLValues :: (QueryFormat queryFormat) => f a -> queryFormat

instance GSQLValues U1 where
  gtoSQLValues U1 = ""

instance (GSQLValues a, GSQLValues b) => GSQLValues (a :*: b) where
  gtoSQLValues (a :*: b) = gtoSQLValues a § gtoSQLValues b  

instance (TextEncoder a, DefaultParamEncoder a) => GSQLValues (M1 i c (Rec0 a)) where
  gtoSQLValues (M1 (K1 val)) = param val

-- instance (SQLValues a) => GSQLValues (M1 i c (Rec0 a)) where
--   gtoSQLValues (M1 (K1 val)) = param val

-- instance (SQLValues a) => GSQLValues (M1 D c a) where
--   gtoSQLValues (M1 (K1 val)) = param val

-- instance (SQLValues a) => GSQLValues (M1 i c (Rec0 a)) where
--   gtoSQLValues (M1 (K1 val)) = toSQLValues x

-- instance (
--     TextEncoder a,
--     DefaultParamEncoder a,
--     TextEncoder b,
--     DefaultParamEncoder b
--     -- TextEncoder f,
--     -- DefaultParamEncoder (f b)
--   ) => GSQLValues (f a :*: f b) where
--   gtoSQLValues (a :*: b) = param a (§) param b

-- class GSQLName (f :: * -> *) where
--   gSQLName :: (QueryFormat queryFormat) => f a -> queryFormat
--   default typename :: (Generic a, GTypeName (Rep a)) => Proxy a -> String
--   typename _proxy = gtypename (from (undefined :: a))

-- instance GSQLName U1 where
--   gSQLName _ = mempty

-- instance (Datatype c) => GSQLName (M1 i c a) where
--   gSQLName (M1 a) = fromString $ datatypeName c