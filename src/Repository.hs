{-# LANGUAGE TemplateHaskell #-}
module Repository (someFunc) where

import Opaleye
import Data.UUID (UUID, nil, toString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

newtype HashedPassword = HashedPassword String

data Credential' a b c = Credential { tenantId :: a, userId :: b, password :: c }
type Credential = Credential' UUID UUID HashedPassword
type CredentialField = Credential' (Field SqlUuid) (Field SqlUuid) (Field SqlText)

$(makeAdaptorAndInstance "pCredential" ''Credential')

credentialTable :: Table CredentialField CredentialField
credentialTable = table "credentialTable" (pCredential Credential { tenantId = tableField "tenantId"
                                                                   , userId = tableField "userId"
                                                                   , password = tableField "password"})

credentialSelect :: Select CredentialField
credentialSelect = selectTable credentialTable

getCredential :: UUID -> UUID -> Select CredentialField
getCredential tenantId userId = do
    row@(Credential tId uId _) <- credentialSelect
    where_ $ (tId .== sqlUUID tenantId) .&& (uId .== sqlUUID userId)
    pure row

someFunc :: IO ()
someFunc = printSql . getCredential nil $ nil

printSql = putStrLn . maybe "Empty select" id . showSql

