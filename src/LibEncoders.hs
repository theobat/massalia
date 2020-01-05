{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LibEncoders where

import Hasql.Encoders
import Data.Functor.Contravariant ((>$<))
import Data.Text
import LibGenericFilter (GQLScalarFilter(..), GQLFilterUUID, GQLFilterText, defaultScalarFilter)
import Data.UUID

-- | This builds an encoder which:
-- | - create a new param accessor if the underlying value is not Nothing
-- | - create an empty param accessor if the underlying value is Nothing
-- |
-- | It has a partial function since it assumes the value used to create the encoder
-- | match the value passed to the encoder subsequently
unsafeMaybeEncodeScalar :: Value a -> Maybe a -> Params (Maybe a)
unsafeMaybeEncodeScalar hasqlValue value = unsafeMaybeEncode value normalEncoder
  where
    normalEncoder = param $ nonNullable hasqlValue

unsafeMaybeEncode :: Maybe a -> Params a -> Params (Maybe a)
unsafeMaybeEncode Nothing _ = mempty
unsafeMaybeEncode (Just _) hasqlValue = unsafeMatcher >$< hasqlValue
  where
    unsafeMatcher Nothing = error "The value passed to the encoder built using this function is wrong. It has a 'Nothing' at a place where the value used to build the encoder had a value" 
    unsafeMatcher (Just v) = v

-- | Takes into account one value from a haskell object. Same as above, 'Maybe value' means 'Nothing' has no effect 
-- | and 'Just a' encodes the value 'a' into the query parameters.
unsafeEncodeObject :: Value filterObjectScalar -> (filterObject -> Maybe filterObjectScalar) -> filterObject -> Params filterObject -> Params filterObject
unsafeEncodeObject hasqlValue accessor filterObjectValue currentParam = case interestingValue of
    Nothing -> currentParam
    Just _ -> currentParam <> newObjectParam
  where
    interestingValue = accessor filterObjectValue
    newScalarParam = unsafeMaybeEncodeScalar hasqlValue interestingValue
    newObjectParam = accessor >$< newScalarParam

unsafeEncodeGQLScalarFilter :: Value scalarType -> Maybe (GQLScalarFilter scalarType b c) -> Params (Maybe (GQLScalarFilter scalarType b c))
unsafeEncodeGQLScalarFilter _ Nothing = mempty
unsafeEncodeGQLScalarFilter hasqlValue (Just filter) = unsafeMaybeEncode (Just filter) isNotEqFilter
  where
    builder accessor = unsafeEncodeObject hasqlValue accessor filter
    isEqFilter = builder isEq mempty
    isNotEqFilter = builder isNotEq isEqFilter

unsafeAddGQLScalarEncoder :: Value scalarType -> (filterObject -> Maybe (GQLScalarFilter scalarType b c)) -> filterObject -> Params filterObject
unsafeAddGQLScalarEncoder value accessor object = accessor >$< accessorParam
  where accessorParam = unsafeEncodeGQLScalarFilter value (accessor object)

plantFilterEncode :: PlantFilter -> Params PlantFilter
plantFilterEncode val = unsafeAddGQLScalarEncoder uuid plantIdFilter val <> unsafeAddGQLScalarEncoder text aRandomText val

  -- unsafeEncodeGQLScalarFilter :: Value scalarType -> Maybe (GQLScalarFilter scalarType) -> 

-- testEncodePlantFilter = unsafeEncodeGQLScalarFilter uuid 

data PlantFilter = PlantFilter {
  plantIdFilter :: Maybe GQLFilterUUID,
  aRandomText :: Maybe GQLFilterText
}

data EncoderFilter a = EncoderFilter (Params a) Text

plantFilterInstance = PlantFilter {
  plantIdFilter=Just $ defaultScalarFilter {
    isEq = fromText "60972711-b4f9-46ce-bbc6-6fb7a788a636"
  },
  aRandomText=Nothing
}

-- unsafeEncodeObject :: 