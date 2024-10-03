{-# LANGUAGE TemplateHaskell #-}
module Repository.PersonRepository where

import Opaleye
import Data.UUID (UUID, nil, toString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.Clock (UTCTime)

type Email = String

data Person' a b c d e f = Person { 
    personId :: a, 
    givenName :: b, 
    middleNames :: c ,
    lastName :: d, 
    email :: e, 
    updatedAt :: f
    }
type Person = Person' UUID String String String Email UTCTime
type PersonField = Person' (Field SqlUuid) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlTimestamptz)

$(makeAdaptorAndInstance "pPerson" ''Person')