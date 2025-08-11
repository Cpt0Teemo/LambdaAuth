module Main (main) where

import JWTSpec (jwtTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

-- Combine all test groups
tests :: TestTree
tests = testGroup "All Tests" [jwtTests]
