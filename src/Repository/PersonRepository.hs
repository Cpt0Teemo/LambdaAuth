{-# LANGUAGE TemplateHaskell #-}
module Repository.PersonRepository where

import Opaleye
import Data.UUID (UUID, nil, toString)
import qualified Data.Text as T
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (Connection)

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


personTable :: Table PersonField PersonField
personTable = table "person"
                       (pPerson Person { personId = tableField "personId"
                                        ,givenName = tableField "givenName"
                                        ,middleNames = tableField "middleNames"
                                        ,lastName = tableField "lastName"
                                        ,email = tableField "email"
                                        ,updatedAt = tableField "updatedAt" })

selectPerson :: Select PersonField
selectPerson = selectTable personTable

selectEmail :: Select (Field SqlVarcharN)
selectEmail = do
    Person _ _ _ _ email _ <- selectPerson
    pure email

test :: Connection -> IO T.Text
test conn = do
    emails <- runSelectI conn selectEmail
    let fstEmail = head emails
    pure fstEmail