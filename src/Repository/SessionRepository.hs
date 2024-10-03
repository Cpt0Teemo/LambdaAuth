{-# LANGUAGE TemplateHaskell #-}
module Repository.SessionRepository where

import Opaleye
import Data.UUID (UUID, nil, toString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.Clock (UTCTime)

data Session' a b c d = Session { 
    sessionId :: a, 
    personId :: b,
    startedAt :: c ,
    expireAt :: d
    }
type Session = Session' UUID UUID UTCTime UTCTime
type SessionField = Session' (Field SqlUuid) (Field SqlUuid) (Field SqlTimestamptz) (Field SqlTimestamptz)

$(makeAdaptorAndInstance "pSession" ''Session')