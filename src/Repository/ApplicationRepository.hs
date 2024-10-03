{-# LANGUAGE TemplateHaskell #-}
module Repository.ApplicationRepository where

import Opaleye
import Data.UUID (UUID, nil, toString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

data Application' a b c = Application { 
    applicationId :: a, 
    applicationSecret :: b, 
    name :: c
    }
type Application = Application' UUID String String
type ApplicationField = Application' (Field SqlUuid) (Field SqlVarcharN) (Field SqlVarcharN)

$(makeAdaptorAndInstance "pApplication" ''Application')