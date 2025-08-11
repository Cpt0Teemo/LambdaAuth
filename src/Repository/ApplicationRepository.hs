{-# LANGUAGE TemplateHaskell #-}

module Repository.ApplicationRepository where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.UUID (UUID, nil, toString)
import Opaleye

data Application' a b c d = Application
  { applicationId :: a,
    applicationSecret :: b,
    name :: c,
    redirectUirs :: d
  }

type Application = Application' UUID String String String

type ApplicationField = Application' (Field SqlUuid) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlText)

$(makeAdaptorAndInstance "pApplication" ''Application')
