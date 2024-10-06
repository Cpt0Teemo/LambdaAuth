{-# LANGUAGE OverloadedStrings #-}
module Main where

import Repository.PersonRepository (test)
import Web.Scotty
import Database.PostgreSQL.Simple
import Debug.Trace
import qualified Data.Text.Lazy as L


main = scotty 3000 $
    get "/user" $ do
        conn  <- liftIO . connect $ localPG
        email <- liftIO . test $ conn
        html $ mconcat ["<h1>Scotty, ", L.fromStrict email, " me up!</h1>"]
    -- get "/:word" $ do
    --     beam <- pathParam "word"
    --     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "LambdaAuth"
        , connectUser = "user"
        , connectPassword = "password"
        }