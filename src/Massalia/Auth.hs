{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Massalia.Auth
-- Description : A module to define utility functions for authentication using JWTs.
module Massalia.Auth (
  module Web.JWT,
  JWTEncodedString(JWTEncodedString)
) where

import Protolude

-- | A JWT claim encoded, the Token in its exchangeable format.
-- No check is performed for integrity (no smart constructor).
newtype JWTEncodedString = JWTEncodedString Text deriving (Eq, Show, Read, Generic)

