-- |
-- Module      : Massalia.HasqlExec
-- Description : A module to reexport all the Execution layer stuff into a single module.
-- Reexports everything from "Hasql.Connection", "Hasql.Session", "Hasql.Statement".
module Massalia.HasqlPool
  ( module Hasql.Pool
  )
where

import Hasql.Pool
