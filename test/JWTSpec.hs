{-# LANGUAGE OverloadedStrings #-}

module JWTSpec where

import Control.Monad.IO.Class (liftIO)
import Crypto.PubKey.RSA.Types
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (isJust)
import Helpers
import JWT
import Test.Tasty
import Test.Tasty.HUnit

-- https://jwt.rocks/ for generating JWT

-- All JWT tests grouped together
jwtTests :: TestTree
jwtTests =
  testGroup
    "JWT Tests"
    [ testCase "Create a valid JWT" testCreateValidJWT,
      testCase "Check a valid JWT signature" testCheckValidJWTSignature
    ]

testCreateValidJWT :: Assertion
testCreateValidJWT = do
  privateKey <- liftIO retrievePrivateKey
  let expectedJwt = Right "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZG1pbiI6dHJ1ZSwiaWF0IjoxNTE2MjM5MDIyLCJuYW1lIjoiSm9obiBEb2UiLCJzdWIiOiIxMjM0NTY3ODkwIn0.Jp1mvUhdXUq66go262L90wZ4ffuxXWlp-dYQOryAR6oeUPPqrDMWdPCZg8Y0Iuuy8Brsl-o6gbUGf3mp4JhRGV1-i0YR5TZZXNNqgFC2JfTxUgmRLthZwQWTLOPpUomQwJo66B6iLzLe7aZBQIfoWiG0G9f5Z9xeSEwGlP6dv9GoyEuVNbrYHkJ2VVnBOAqzfkdjqeWKkdMJxzFW1YSa8jBh3sQAEytQg5kAHk6MgrDq8MU9ybPMyg2JahlSMTXbvleH7OIf20CwhBrJZ0T9JAjAvfv3Fd7hmSjpDJDJ3bUj5Nls6T5BEqMXqD1zvs4CQFPsuUFi-tAZ4pVuuwrgAQ"
  let jwtMonad = do
        addStringClaim "sub" "1234567890"
        addStringClaim "name" "John Doe"
        addBoolClaim "admin" True
        addNumberClaim "iat" 1516239022
  jwt <- liftIO . toToken privateKey $ jwtMonad
  assertEqual "JWT token should match expected value" expectedJwt jwt

testCheckValidJWTSignature :: Assertion
testCheckValidJWTSignature = do
  privateKey <- liftIO retrievePrivateKey
  let publicKey = retrievePublicKey privateKey
  let jwt = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZG1pbiI6dHJ1ZSwiaWF0IjoxNTE2MjM5MDIyLCJuYW1lIjoiSm9obiBEb2UiLCJzdWIiOiIxMjM0NTY3ODkwIn0.Jp1mvUhdXUq66go262L90wZ4ffuxXWlp-dYQOryAR6oeUPPqrDMWdPCZg8Y0Iuuy8Brsl-o6gbUGf3mp4JhRGV1-i0YR5TZZXNNqgFC2JfTxUgmRLthZwQWTLOPpUomQwJo66B6iLzLe7aZBQIfoWiG0G9f5Z9xeSEwGlP6dv9GoyEuVNbrYHkJ2VVnBOAqzfkdjqeWKkdMJxzFW1YSa8jBh3sQAEytQg5kAHk6MgrDq8MU9ybPMyg2JahlSMTXbvleH7OIf20CwhBrJZ0T9JAjAvfv3Fd7hmSjpDJDJ3bUj5Nls6T5BEqMXqD1zvs4CQFPsuUFi-tAZ4pVuuwrgAQ"
  let result = fromToken publicKey jwt
  assertBool "Returns valid claims" (isJust result)

retrievePublicKey :: PrivateKey -> PublicKey
retrievePublicKey = toPublicKey . KeyPair

retrievePrivateKey :: IO PrivateKey
retrievePrivateKey = do
  eitherPrivateKey <- readPrivateKeyFromPem "./test/test_private_key.pem"
  case eitherPrivateKey of
    Left errorStr -> putStrLn errorStr >> fail "Couldn't find private key, aborting"
    Right key -> return key
