{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaSchema.Industry.DBContext where

import Protolude
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Void (Void)
import Data.Aeson (FromJSON)
import MassaliaSchema.Industry.PlantInput (PlantInput)
import qualified MassaliaSchema.Industry.PlantInput as PlantInput
import MassaliaSchema.Industry.TruckInput (TruckInput)
import qualified MassaliaSchema.Industry.TruckInput as TruckInput

data DBContext containerType typeContext = DBContext {
  plant :: Apply typeContext containerType PlantInput,
  truck :: Apply typeContext containerType TruckInput
} deriving (Generic)

class Aggregable rec where
  toQueryFormat :: rec -> Text

type family Apply token (containerType :: * -> *) underlyingType
type instance Apply Text containerType underlyingType = underlyingType -> Text


data PlantInputListToken
type instance Apply PlantInputListToken containerType underlyingType = PlantInputList containerType underlyingType

type family PlantInputList containerType underlyingType where
  PlantInputList containerType PlantInput = containerType PlantInput
  PlantInputList _ _ = Maybe Void

type Test = DBContext [] PlantInputListToken
type TestGathered = DBContext Maybe Text

deriving instance FromJSON (DBContext [] PlantInputListToken)

test :: Test
test = DBContext {
  plant = [],
  truck = Nothing
}
testG :: TestGathered
testG = DBContext {
  plant = const "",
  truck = const ""
}

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

