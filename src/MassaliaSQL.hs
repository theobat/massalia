{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSQL (
  dynamicallyParameterizedStatement,
  dynamicallyParameterized,
  module Session
) where

import MassaliaQueryFormat (
    TextEncoder,
    DefaultParamEncoder,
    QueryFormat(param, fromText), HasqlSnippet
  )

import Hasql.DynamicStatements.Session (dynamicallyParameterizedStatement)
import Hasql.DynamicStatements.Statement (dynamicallyParameterized)
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement

comma :: QueryFormat formatType => formatType -> formatType -> formatType
comma a b = a <> "," <> b

commaP :: (TextEncoder a, DefaultParamEncoder a, QueryFormat formatType) => a -> formatType -> formatType
commaP paramToEncode = comma (param paramToEncode) 

