{-# LANGUAGE TemplateHaskell #-}

module Repository.LogRepository where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.UUID (UUID, nil, toString)
import Opaleye

type JsonData = String

data Log' a b c = Log
  { logId :: a,
    logType :: b,
    jsonData :: c
  }

type Log = Log' UUID String JsonData

type LogField = Log' (Field SqlUuid) (Field SqlVarcharN) (Field SqlJsonb)

$(makeAdaptorAndInstance "pLog" ''Log')
