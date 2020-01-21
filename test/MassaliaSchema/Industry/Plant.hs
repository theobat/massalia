{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Industry.Plant (
    Plant(..)
) where

import Data.UUID (UUID)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Data (Data)
import Industry.Truck (Truck)

-- import Truck (Truck)

data Plant = Plant {
  id :: UUID
} deriving (Show, Generic, Data)