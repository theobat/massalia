{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MassaliaSchema.Industry.DBContext where

import Data.Aeson (FromJSON)
import Data.Morpheus.Types (GQLRequest, GQLResponse, GQLRootResolver (..), GQLType, IORes, QUERY, ResolveQ, Resolver, Undefined (..))
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Void (Void)
import MassaliaSchema.Industry.PlantInput (PlantInput)
import qualified MassaliaSchema.Industry.PlantInput as PlantInput
import MassaliaSchema.Industry.TruckInput (TruckInput)
import qualified MassaliaSchema.Industry.TruckInput as TruckInput
import Protolude

data DBContext containerType typeContext
  = DBContext
      { plant :: Apply typeContext (containerType PlantInput),
        truck :: Apply typeContext (containerType TruckInput),
        ok :: Apply typeContext (containerType TruckInput)
      }
  deriving (Generic)

type family Apply token someType

data PlantInputListToken

type instance Apply PlantInputListToken someType = PlantInputList someType

type family PlantInputList someType where
  PlantInputList (containerType PlantInput) = containerType PlantInput
  PlantInputList _ = Maybe Void

newtype PlantInputDBContext = PlantInputDBContext (DBContext [] PlantInputListToken)

data SimpleDedupeStruct a = SimpleDedupeStruct a UUID

data FullDedupeStruct a = FullDedupeStruct a a

type PlantInputDBContextDedupe = DBContext SimpleDedupeStruct PlantInputListToken

type TestGathered = DBContext Maybe Text

deriving instance FromJSON (DBContext [] PlantInputListToken)

deriving instance GQLType (DBContext [] PlantInputListToken)

test :: PlantInputDBContext
test =
  PlantInputDBContext $
    DBContext
      { plant = [],
        truck = Nothing,
        ok = Nothing
      }

something :: (Foldable containerType) => DBContext containerType typeContext -> Text
something dbContext = ""
  where
    -- okoko = foldr undefined "" plantTest
    plantTest = plant dbContext

-- testGene :: forall a. Eq a => a -> Text
-- testGene = case level1 of
--   M1 a -> case a of
--     M1 b -> case b of
--       M1 (K1 c) :*: d -> undefined
--   where
--     level1 = from test

plantInsert dbConnection queryArgs = do
  -- Context { currentSelection = selection } <- unsafeInternalContext
  lift

-- iterate input = case input of
--   M1 (K1 c) :*: d -> iterate d
-- _ -> undefined

-- data DBContext typeContext containerType = DBContext {
--   plant :: containerType (TLMaybe typeContext PlantInput),
--   truck :: containerType (TLMaybe typeContext TruckInput)
-- }

-- class DBContextInstance

-- | Closed type family (bad):
-- type family TLMaybe typeIndicator (container :: * -> *) underlyingType where
--   TLMaybe Ok container PlantInput = container PlantInput
--   TLMaybe Full container anyType = container anyType -- | This is for a full DBContext, every entity is welcome
--   TLMaybe _ _ _ = Maybe Void

-- | Open type family (not working):
-- type family TLMaybe typeIndicator (container :: * -> *) underlyingType

-- type instance TLMaybe Ok container PlantInput = container PlantInput
-- type instance TLMaybe Ok container ss = container Void

-- | Class embedded type family :

-- class (Monoid (container underlyingType)) => Collects typeIndicator container underlyingType where
--   type TLMaybe typeIndicator (container :: * -> *) underlyingType

-- type family
-- instance Collects Ok [] PlantInput where
--   type TLMaybe Ok [] PlantInput = [] PlantInput
-- type TLMaybe Ok container ss = container Void

-- type instance TLMaybe Ok container PlantInput = container PlantInput
-- type instance TLMaybe Ok container ss = container Void

-- instance (Monoid (container underlyingType)) => Monoid (TLMaybe typeIndicator (container :: * -> *) underlyingType) where
--   mempty =

data Full

data Ok
-- testValue :: (forall a. Monoid (TLMaybe typeContext containerType a)) => DBContext typeContext containerType
-- testValue = DBContext {
--   plant = mempty,
--   truck = mempty
-- }
