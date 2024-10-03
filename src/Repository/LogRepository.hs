{-# LANGUAGE TemplateHaskell #-}
module Repository.LogRepository where

import Opaleye
import Data.UUID (UUID, nil, toString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

type JsonData = String

data Log' a b c = Log { 
    logId :: a, 
    logType :: b, 
    jsonData :: c
    }
type Log = Log' UUID String JsonData
type LogField = Log' (Field SqlUuid) (Field SqlVarcharN) (Field SqlJsonb)

$(makeAdaptorAndInstance "pLog" ''Log')