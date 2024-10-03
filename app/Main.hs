module Main where

import qualified Repository.Repository as Repository (someFunc) 

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Repository.someFunc
