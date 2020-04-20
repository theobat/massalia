-- |
-- Module      : Massalia.HasqlDec
-- Description : A module to reexport all the sql decoders from "Hasql.Decoders". This is simply meant to avoid
--  to avoid importing hasql again in the consumer project.
module Massalia.HasqlDec
  ( module Hasql.Decoders,
  )
where

import Hasql.Decoders
