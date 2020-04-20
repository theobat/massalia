{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MassaliaSchema.Industry.Organization
  ( Organization (..),
  )
where

import Data.Data (Data)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- import Plant (Plant)

data Organization
  = Organization
      { id :: UUID
      }
  deriving (Show, Generic, Data)
