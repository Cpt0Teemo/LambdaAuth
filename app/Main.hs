{-# LANGUAGE OverloadedStrings #-}
module Main where

import Repository.PersonRepository (test)
import Web.Scotty
import Database.PostgreSQL.Simple
import Debug.Trace
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO (readFile)


main = scotty 3000 $ do
    get "/user" $ do
        conn  <- liftIO . connect $ localPG
        email <- liftIO . test $ conn
        html $ mconcat ["<h1>Scotty, ", L.fromStrict email, " me up!</h1>"]

    get "/login" $ do
        loginPage <- liftIO . LIO.readFile $ "app/LoginPage.html"
        html loginPage

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "LambdaAuth"
        , connectUser = "user"
        , connectPassword = "password"
        }