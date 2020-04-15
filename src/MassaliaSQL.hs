{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSQL where

import MassaliaQueryFormat (
    TextEncoder,
    DefaultParamEncoder,
    QueryFormat(param, fromText), HasqlSnippet
  )

comma :: QueryFormat formatType => formatType -> formatType -> formatType
comma a b = a <> "," <> b

commaP :: (TextEncoder a, DefaultParamEncoder a, QueryFormat formatType) => a -> formatType -> formatType
commaP paramToEncode = comma (param paramToEncode) 
