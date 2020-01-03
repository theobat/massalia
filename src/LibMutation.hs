{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LibMutation
where

import Data.UUID
import qualified Data.Text as T


testInsert :: T.Text -> T.Text
testInsert = undefined


data WorkInput = WorkInput {
    id :: UUID
}
data WorkAccessInput = WorkAccessInput {
    workAccessId :: UUID,
    workId :: UUID
}

data DbContext = DbContext {
    work :: [WorkInput],
    workAccess :: [WorkAccessInput]
}
