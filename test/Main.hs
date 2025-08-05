module Main (main) where

import Test.Tasty
import JWTSpec (jwtTests)

main :: IO ()
main = defaultMain tests

-- Combine all test groups
tests :: TestTree
tests = testGroup "All Tests" [jwtTests]
