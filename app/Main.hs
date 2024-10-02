module Main where

import qualified Repository (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Repository.someFunc
