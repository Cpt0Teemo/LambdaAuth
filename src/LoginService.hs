module LoginService where

import Database.PostgreSQL.Simple (Connection)
import Repository.PersonRepository (Person, password, UnencryptedPassword, EncryptedPassword)

encryptPassword :: UnencryptedPassword -> EncryptedPassword
encryptPassword = id

createUser :: Connection -> Person -> UnencryptedPassword -> IO Bool
createUser = undefined

verifyUserPassword :: Person -> UnencryptedPassword -> Bool
verifyUserPassword person unencryptedPassword = personPassword == encryptedPassword
    where
        personPassword = password person
        encryptedPassword = encryptPassword unencryptedPassword