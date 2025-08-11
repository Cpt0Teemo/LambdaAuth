{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (liftM, mfilter)
import Data.Bifunctor
import Data.Either.Extra (maybeToEither)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.IO qualified as LIO (readFile)
import Database.PostgreSQL.Simple
import Helpers
import JWT
import LoginService
import Network.HTTP.Types.Status
import Repository.PersonRepository (Person, UnencryptedPassword, email, getUserByEmail, test)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/user" $ do
    conn <- liftIO . connect $ localPG
    email <- liftIO . test $ conn
    html $ mconcat ["<h1>Scotty, ", L.fromStrict email, " me up!</h1>"]

  get "/.wellknown/jwk.json" $ do
    publicKey <- publicKeyToJWK <$> retrievePublicKey <$> retrievePrivateKey
    status status200
    json publicKey

  get "/login" $ do
    loginPage <- liftIO . LIO.readFile $ "app/LoginPage.html"
    html loginPage

  post "/login" $ do
    privateKey <- retrievePrivateKey
    conn <- liftIO . connect $ localPG
    maybeUsername :: Maybe String <- formParamMaybe "username"
    maybePassword :: Maybe String <- formParamMaybe "password"
    case (maybeUsername, maybePassword) of
      (Just username, Just password) -> do
        eitherUser <- liftIO . fmap (maybeToEither "Couldn't find User") . getUserByEmail conn $ username
        case eitherUser of
          Right user ->
            if verifyUserPassword password user
              then liftIO (createUserJwt privateKey user) >>= loginUser
              else loginUser (Left "Invalid password")
          Left err -> loginUser (Left err)
      _ -> loginUser (Left "Missing username or password")

loginUser :: Either a JwtToken -> ActionM ()
loginUser (Right token) = status status200 >> text (bsToLazyText token)
loginUser _ = status status404 >> text "Username and password do not match a user"

retrievePrivateKey :: ActionM PrivateKey
retrievePrivateKey = do
  eitherPrivateKey <- liftIO . readPrivateKeyFromPem $ "private_key.pem"
  case eitherPrivateKey of
    Left errorStr -> liftIO $ putStrLn errorStr >> fail "Couldn't find private key, aborting"
    Right key -> return key

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectDatabase = "LambdaAuth",
      connectUser = "user",
      connectPassword = "password"
    }
