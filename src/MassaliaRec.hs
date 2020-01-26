{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MassaliaRec where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Data.Text (Text, pack)
import MassaliaCore (MassaliaStruct(..))

colValueToText fieldName _ getter _ (currentTxt, currentRec) = (currentTxt <> ", " <> fieldName <> "=" <> (pack $ show $ getter currentRec), currentRec)

instance MassaliaStruct (,) Text record where
  getInitialValue (name, _) defaultRec = (name, defaultRec)
  simpleCol = colValueToText
  exprCol (fieldName, _) = colValueToText fieldName
  subColWrap = undefined
  
