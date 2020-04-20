-- |
-- Module      : Massalia.HasqlEnc
-- Description : A module to reexport all the sql encoders from "Hasql.Encoders". This is simply meant to avoid
--  to avoid importing hasql again in the consumer project.
module Massalia.HasqlEnc
  ( module Hasql.Encoders,
  )
where

import Hasql.Encoders
