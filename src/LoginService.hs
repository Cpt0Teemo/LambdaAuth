module LoginService where

import Database.PostgreSQL.Simple (Connection)
import Repository.PersonRepository (Person, password, UnencryptedPassword, EncryptedPassword, email)
import JWT

encryptPassword :: UnencryptedPassword -> EncryptedPassword
encryptPassword = id

createUser :: Connection -> Person -> UnencryptedPassword -> IO Bool
createUser = undefined

verifyUserPassword :: UnencryptedPassword -> Person -> Bool
verifyUserPassword unencryptedPassword person = personPassword == encryptedPassword
    where
        personPassword = password person
        encryptedPassword = encryptPassword unencryptedPassword

createUserJwt :: MonadRandom m => PrivateKey -> Person -> m (Either Error JwtToken)
createUserJwt privateKey user = toToken privateKey $ do
    addStringClaim "iss" "LambdaAuth"
    addStringClaim "email" (email user)
    addNumberClaim "exp" 1516239022