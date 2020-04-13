{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MassaliaError
  ( MassaliaError,
  )
where

import Protolude

data MassaliaError =
  SQLBuildSelectError Text |
  HasqlError Text
