{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.QueryFormat
-- Description : A module to define a query format __interface__ for SQL queries. In Hasql, we use dynamic queries
--  because we don't know in advance what parameters will be provided or not by the graphql query. The query format
--  provided is made for communicating efficiently with Postgres so it has no instance of 'Show' or 'Eq' which can be
--  a problem for testing and for debugging. This module defined a typeclass to abstract whatever Type is used for the
--  query formatting.
module Massalia.QueryFormat
  ( SQLEncoder (sqlEncode),
    FromText(fromText),
    IsString(fromString),
    DefaultParamEncoder,
    HasqlSnippet,
    commaAssemble,
    (§),
    takeParam,
    takeMaybeParam,
    Snippet.param,
    inSingleQuote,
    inParens
  )
where

import Data.Foldable (foldr1)
import Data.Int (Int64)
import Data.Sequence (Seq)
import Data.String (String, IsString)
import qualified Data.String as String (IsString (fromString))
import Data.Text (Text, pack, replace, unpack)
import Data.UUID hiding (fromText, fromString)
import Data.Vector (Vector)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Encoders (NullableOrNot, Value)
import Hasql.Implicits.Encoders (DefaultParamEncoder (defaultParam))
import Massalia.Utils (EmailAddress, LocalTime, intercalate, intercalateMap, emailToText)
import Protolude hiding (intercalate, replace)

type HasqlSnippet = Snippet

(§) :: (IsString content, Monoid content) => content -> content -> content
(§) a b = a <> "," <> b

commaAssemble :: (IsString a, Monoid a) => [a] -> a
commaAssemble = intercalateMap identity ","

-- | Yields a query formatted snippet with the given parameter encoded as a parameter in the query format.
takeParam ::
  (SQLEncoder paramValueType content) =>
  (recordType -> paramValueType) ->
  recordType ->
  content
takeParam accessor record = sqlEncode $ accessor record

takeMaybeParam ::
  (SQLEncoder paramValueType content) =>
  (recordType -> Maybe paramValueType) ->
  recordType ->
  content ->
  content
takeMaybeParam accessor record defaultValue = case (accessor record) of
  Nothing -> defaultValue
  Just value -> sqlEncode value



class FromText content where
  fromText :: Text -> content
instance FromText Text where
  fromText = identity
instance FromText String where
  fromText = unpack
instance FromText Snippet where
  fromText = String.fromString . unpack

class (IsString queryFormat, Monoid queryFormat, FromText queryFormat) =>
  SQLEncoder underlyingType queryFormat where
  sqlEncode :: underlyingType -> queryFormat

instance SQLEncoder Void Text where
  sqlEncode = mempty
instance (SQLEncoder a Text) => SQLEncoder (Maybe a) Text where
  sqlEncode = maybe "null" sqlEncode
instance (SQLEncoder a Snippet) => SQLEncoder (Maybe a) Snippet where
  sqlEncode = maybe "null" sqlEncode
instance (SQLEncoder a Text) => SQLEncoder [a] Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder [a]) => SQLEncoder [a] Snippet where
  sqlEncode = Snippet.param
instance (SQLEncoder a Text) => SQLEncoder (Vector a) Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder (Vector a)) => SQLEncoder (Vector a) Snippet where
  sqlEncode = Snippet.param
instance (SQLEncoder a Text) => SQLEncoder (Seq a) Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder (Seq a)) => SQLEncoder (Seq a) Snippet where
  sqlEncode = Snippet.param
instance (SQLEncoder a Text) => SQLEncoder (Set a) Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder (Set a)) => SQLEncoder (Set a) Snippet where
  sqlEncode = Snippet.param

-- instance (Foldable collection, DefaultParamEncoder (collection elementType)) =>
--   SQLEncoder (collection elementType) Snippet where
--   sqlEncode = Snippet.param

instance SQLEncoder UUID Text where
  sqlEncode = inSingleQuote . toText
instance SQLEncoder UUID Snippet where
  sqlEncode = Snippet.param
  
instance SQLEncoder Text Text where
  sqlEncode = inSingleQuote
instance SQLEncoder Text Snippet where
  sqlEncode = Snippet.param
  
instance SQLEncoder Int64 Text where
  sqlEncode = pack . show
instance SQLEncoder Int64 Snippet where
  sqlEncode = Snippet.param

instance SQLEncoder EmailAddress Text where
  sqlEncode = pack . show
instance SQLEncoder EmailAddress Snippet where
  sqlEncode = Snippet.param . emailToText 

inSingleQuote a = "'" <> a <> "'"
inParens a = "(" <> a <> ")"

collectionTextEncode :: (Foldable collection, SQLEncoder underlyingType Text) =>
  collection underlyingType -> Text
collectionTextEncode collection = wrapCollection assembled
  where
    assembled = foldr assemblerFun "" collection
    assemblerFun :: SQLEncoder b Text => b -> Text -> Text
    assemblerFun val "" = rawEncode val
    assemblerFun val currentEncoded = currentEncoded <> "," <> rawEncode val
    rawEncode val = replace "'" "" $ sqlEncode val
    wrapCollection a = "'{" <> a <> "}'"
    