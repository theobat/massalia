{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Module      : Massalia.Default
-- Description : A module to define a set of default values for
-- the SQLDefault table logic.
module Massalia.Default
where

import Massalia.Utils (
    uuidNil,  
    emailDefault,
    localTimeDefault,
    dayDefault,
    utcTimeDefault,
  )

uuid = uuidNil
email = emailDefault
timestamptz = localTimeDefault
localTime = localTimeDefault
utc = utcTimeDefault
date = dayDefault
int :: Num a => a
int = 0

monoid :: (Monoid a) => a
monoid = mempty
