{-# LANGUAGE OverloadedStrings #-}
module Main where

import Repository.PersonRepository (test, Person, email, getUserByEmail, UnencryptedPassword)
import Web.Scotty
import Network.HTTP.Types.Status
import Database.PostgreSQL.Simple
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO (readFile)
import LoginService


main = scotty 3000 $ do
    get "/user" $ do
        conn  <- liftIO . connect $ localPG
        email <- liftIO . test $ conn
        html $ mconcat ["<h1>Scotty, ", L.fromStrict email, " me up!</h1>"]

    get "/login" $ do
        loginPage <- liftIO . LIO.readFile $ "app/LoginPage.html"
        html loginPage

    post "/login" $ do
        conn  <- liftIO . connect $ localPG
        maybeUsername :: Maybe String <- formParamMaybe "username"
        maybePassword :: Maybe String <- formParamMaybe "password"
        case (maybeUsername, maybePassword) of
            (Just username, Just password) -> do
                maybeUser <- liftIO . getUserByEmail conn $ username
                loginUser . isUserPasswordCorrect maybeUser $ password
            _ -> status status400 >> text "Missing username or password"

loginUser :: Bool -> ActionM ()
loginUser True = status status200 >> text "User credentials are correct"
loginUser False = status status404 >> text "Username and password do not match a user"

isUserPasswordCorrect :: Maybe Person -> UnencryptedPassword -> Bool
isUserPasswordCorrect (Just user) password = verifyUserPassword user password
isUserPasswordCorrect Nothing _ = False

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectDatabase = "LambdaAuth"
        , connectUser = "user"
        , connectPassword = "password"
        }