{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Massalia.MRange where

import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson as JSON
import Data.Aeson
  (
    FromJSON (..),
    ToJSON,
    toJSON1,
    withArray,
  )
import Data.Aeson.Types
  (
    Parser,
  )
import Data.Range
  ( Bound (Bound),
    BoundType
      ( Exclusive,
        Inclusive
      ),
    Range
      ( InfiniteRange,
        LowerBoundRange,
        SingletonRange,
        SpanRange,
        UpperBoundRange,
        InfiniteRange
      ),
    inRange,
  )
import qualified Data.Vector as Vector
import Protolude hiding (Enum)

newtype MRange rangeType = MRange (Range rangeType) deriving (Eq, Show, Functor)

isInRange :: Ord rangeType => MRange rangeType -> rangeType -> Bool
isInRange (MRange range) = inRange range
infiniteRange :: MRange rangeType
infiniteRange = MRange InfiniteRange
singletonRange :: rangeType -> MRange rangeType
singletonRange singletonValue = MRange $ SingletonRange singletonValue

instance (FromJSON rangeType) => FromJSON (MRange rangeType) where
  parseJSON val = MRange <$> withArray "Range" jsonRangeParser val

instance (ToJSON rangeType) => ToJSON (MRange rangeType) where
  toJSON (MRange val) = toJSON1 $ rangeToJSON val

rangeToJSON :: (ToJSON rangeType) => Range rangeType -> [JSON.Value]
rangeToJSON range = case range of
  InfiniteRange -> [JSON.Null, JSON.Null, JSON.Null, JSON.Null]
  SingletonRange single ->
    [JSON.toJSON single, JSON.toJSON single, JSON.Null, JSON.Null]
  LowerBoundRange (Bound lower _) ->
    [JSON.toJSON lower, JSON.Null, JSON.Null, JSON.Null]
  UpperBoundRange (Bound upper _) ->
    [JSON.Null, JSON.toJSON upper, JSON.Null, JSON.Null]
  SpanRange (Bound lower _) (Bound upper _) ->
    [JSON.toJSON lower, JSON.toJSON upper, JSON.Null, JSON.Null]

boundTypeToJSON :: Bool -> BoundType -> JSON.Value
boundTypeToJSON True Inclusive = "["
boundTypeToJSON True Exclusive = "("
boundTypeToJSON False Inclusive = "]"
boundTypeToJSON False Exclusive = ")"

jsonRangeParser :: (FromJSON rangeType) => JSON.Array -> Parser (Range rangeType)
jsonRangeParser array = parseTriple arrayValues
  where
    getValueAtIndex = (Vector.!?)
    arrayValues = getValueAtIndex array <$> Triple 0 1 2

parseTriple :: (FromJSON rangeType) => Triple (Maybe JSON.Value) -> Parser (Range rangeType)
parseTriple triplet = case fromMaybe JSON.Null <$> triplet of
  Triple JSON.Null JSON.Null _ -> pure InfiniteRange
  Triple lowerBound JSON.Null inclusivity ->
    LowerBoundRange
      <$> parseBound lowerBound (parseJSONBoundType "[)" inclusivity) fst
  Triple JSON.Null upperBound inclusivity ->
    UpperBoundRange
      <$> parseBound upperBound (parseJSONBoundType "(]" inclusivity) snd
  Triple lowerBound upperBound inclusivity ->
    SpanRange
      <$> parseBound lowerBound parserBoundType fst
      <*> parseBound upperBound parserBoundType snd
    where
      parserBoundType = parseJSONBoundType "[)" inclusivity

parseBound ::
  (FromJSON rangeType) =>
  JSON.Value ->
  (Parser (BoundType, BoundType)) ->
  ((BoundType, BoundType) -> BoundType) ->
  Parser (Bound rangeType)
parseBound bound inclusivity accessor =
  Bound <$> parseJSON bound <*> (accessor <$> inclusivity)

parseJSONBoundType :: Text -> JSON.Value -> Parser (BoundType, BoundType)
parseJSONBoundType defValue JSON.Null = parseBoundType defValue
parseJSONBoundType _ value = JSON.withText "BoundType" parseBoundType value

parseBoundType :: Text -> Parser (BoundType, BoundType)
parseBoundType input = case input of
  -- POSTGRES SYNTAX
  "[]" -> pure (Inclusive, Inclusive)
  "[)" -> pure (Inclusive, Exclusive)
  "(]" -> pure (Exclusive, Inclusive)
  "()" -> pure (Exclusive, Exclusive)
  -- MATH SYNTAX
  "[[" -> pure (Inclusive, Exclusive)
  "]]" -> pure (Exclusive, Inclusive)
  "][" -> pure (Exclusive, Exclusive)
  -- EXPLICIT SYNTAX
  "InclusiveInclusive" -> pure (Inclusive, Inclusive)
  "InclusiveExclusive" -> pure (Inclusive, Exclusive)
  "ExclusiveInclusive" -> pure (Exclusive, Inclusive)
  "ExclusiveExclusive" -> pure (Exclusive, Exclusive)
  -- SINGLE SYNTAX
  "Inclusive" -> pure (Inclusive, Inclusive)
  "Exclusive" -> pure (Exclusive, Exclusive)
  _ ->
    Fail.fail
      "The allowed syntax is [,],(,),Inclusive,Exclusive for Bounds, [] meaning inclusive and () meaning exclusive"
      
data Triple a = Triple a a a deriving (Eq, Show, Functor)
