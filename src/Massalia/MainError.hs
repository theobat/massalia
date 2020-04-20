{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.MainError
-- Description : Unused yet. An error type to replace all the undefined/panic/error in the current code base.
module Massalia.MainError
  ( MassaliaError,
  )
where

import Protolude

data MassaliaError
  = SQLBuildSelectError Text
  | HasqlError Text
