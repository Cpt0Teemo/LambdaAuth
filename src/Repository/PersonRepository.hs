{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Repository.PersonRepository where

import Control.Exception
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Opaleye

type Email = String

type UnencryptedPassword = String

type EncryptedPassword = String

newtype MoreThanOneUser = MoreThanOneUser Email deriving (Show)

instance Exception MoreThanOneUser

data Person' a b c d e f g = Person
  { personId :: a,
    givenName :: b,
    middleNames :: c,
    lastName :: d,
    email :: e,
    password :: f,
    updatedAt :: g
  }
  deriving (Show)

type Person = Person' UUID String String String Email EncryptedPassword UTCTime

type PersonField = Person' (Field SqlUuid) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlVarcharN) (Field SqlTimestamptz)

$(makeAdaptorAndInstance "pPerson" ''Person')

personTable :: Table PersonField PersonField
personTable =
  table
    "person"
    ( pPerson
        Person
          { personId = tableField "personId",
            givenName = tableField "givenName",
            middleNames = tableField "middleNames",
            lastName = tableField "lastName",
            email = tableField "email",
            password = tableField "password",
            updatedAt = tableField "updatedAt"
          }
    )

selectPerson :: Select PersonField
selectPerson = selectTable personTable

selectEmail :: Select (Field SqlVarcharN)
selectEmail = do
  Person _ _ _ _ email _ _ <- selectPerson
  pure email

selectByEmail :: Email -> Select PersonField
selectByEmail email = do
  person@(Person _ _ _ _ fieldEmail _ _) <- selectPerson
  where_ $ sqlStringVarcharN email .== fieldEmail
  pure person

getUserByEmail :: Connection -> Email -> IO (Maybe Person)
getUserByEmail conn email = do
  users <- runSelect conn (selectByEmail email)
  case users of
    [] -> pure Nothing
    [x] -> pure (Just x)
    _ -> throwIO (MoreThanOneUser email)

test :: Connection -> IO T.Text
test conn = do
  emails <- runSelectI conn selectEmail
  let fstEmail = head emails
  pure fstEmail
