{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSchema.Industry.Truck (
    Truck(..),
    truckInitSQL
) where
import Prelude hiding(id)
import qualified Prelude(id) 
import Data.UUID (UUID, nil)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import MassaliaCore (MassaliaStruct(..))
import qualified Hasql.Decoders as Decoders
import Data.Morpheus.Types.Internal.AST.Selection (ValidSelection, ValidSelectionSet)
import Data.Morpheus.Types.Internal.AST.Base (Key)
  
data Truck = Truck {
  id :: UUID
  , vehicleId :: Text
} deriving (Show, Generic)


-- | change interface for simpleCol fieldName ...
-- | exprCol (fieldName, sqlName)
truckSelect :: MassaliaStruct wrapper someType Truck => (Key, ValidSelection) -> wrapper someType Truck -> wrapper someType Truck
truckSelect (fieldName, _) = case fieldName of
  "id" -> simpleCol fieldName (\e v -> e{id=v}) id Decoders.uuid
  "vehicleId" -> simpleCol fieldName (\e v -> e{vehicleId=v}) vehicleId Decoders.text
  _ -> Prelude.id
  -- where
  --   snake = 

-- | A function to take into account all the available filters for truck entities
truckFilter :: MassaliaStruct wrapper someType Truck => Text -> wrapper someType Truck -> wrapper someType Truck
truckFilter = undefined

truckInitSQL :: MassaliaStruct wrapper someType Truck => ValidSelectionSet -> wrapper someType Truck
truckInitSQL = foldr truckSelect initialValue
  where initialValue = getInitialValue (dupe "truck") (Truck{ id=nil, vehicleId="" }) 

dupe x = (x,x)