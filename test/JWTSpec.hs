{-# LANGUAGE OverloadedStrings #-}
module JWTSpec where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as BS
import JWT

-- https://jwt.rocks/ for generating JWT

testSecret :: BS.ByteString
testSecret = "a-string-secret-at-least-256-bits-long"

-- All JWT tests grouped together
jwtTests :: TestTree
jwtTests = testGroup "JWT Tests" 
  [ testCase "Create a valid JWT" testCreateValidJWT
  , testCase "Check a valid JWT signature" testCheckValidJWTSignature
  , testCase "Check an invalid JWT signature" testCheckInvalidJWTSignature
  ]

testCreateValidJWT :: Assertion
testCreateValidJWT = do
    let expectedJwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZG1pbiI6dHJ1ZSwiaWF0IjoxNTE2MjM5MDIyLCJuYW1lIjoiSm9obiBEb2UiLCJzdWIiOiIxMjM0NTY3ODkwIn0.0JhgHi-tv1J-lFsc5DD1__104EfxNGMnQ95SHiwgeL8"
    let jwtMonad = do
            addStringClaim "sub" "1234567890"
            addStringClaim "name" "John Doe"
            addBoolClaim "admin" True
            addNumberClaim "iat" 1516239022
    let jwt = toToken testSecret jwtMonad
    assertEqual "JWT token should match expected value" expectedJwt jwt

testCheckValidJWTSignature :: Assertion
testCheckValidJWTSignature = do
    let jwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZG1pbiI6dHJ1ZSwiaWF0IjoxNTE2MjM5MDIyLCJuYW1lIjoiSm9obiBEb2UiLCJzdWIiOiIxMjM0NTY3ODkwIn0.0JhgHi-tv1J-lFsc5DD1__104EfxNGMnQ95SHiwgeL8"
    assertBool "JWT signature should be valid" (isValid testSecret jwt)

testCheckInvalidJWTSignature :: Assertion
testCheckInvalidJWTSignature = do
    let jwtMadeWithDiffSignature = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhZG1pbiI6dHJ1ZSwiaWF0IjoxNTE2MjM5MDIyLCJuYW1lIjoiSm9obiBEb2UiLCJzdWIiOiIxMjM0NTY3ODkwIn0.NlPUPF45faG4VCR1WRGCcFnbVxD4AT3a580IYPqq0rc"
    assertBool "JWT signature should be invalid" (not $ isValid testSecret jwtMadeWithDiffSignature)

