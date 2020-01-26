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
                                                  
instance MassaliaStruct (,) Text record where
  simpleCol _ _ getter _ (currentTxt, currentRec) = (currentTxt <> " " <> (pack $ show $ getter currentRec), currentRec)
  
