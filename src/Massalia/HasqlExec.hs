-- |
-- Module      : Massalia.HasqlExec
-- Description : A module to reexport all the Execution layer stuff into a single module.
-- Reexports everything from "Hasql.Connection", "Hasql.Session", "Hasql.Statement".
module Massalia.HasqlExec
  ( module Hasql.Connection,
    module Hasql.Session,
    module Hasql.Statement,
    dynamicallyParameterizedStatement,
    dynamicallyParameterized,
  )
where

import Hasql.Connection
import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import Hasql.Session
import Hasql.Statement
