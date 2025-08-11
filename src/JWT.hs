{-# LANGUAGE OverloadedStrings #-}

module JWT (toToken, fromToken, addStringClaim, addBoolClaim, addNumberClaim, JwtToken, PrivateKey, PublicKey, MonadRandom, Error) where

import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA.PKCS15 (HashAlgorithmASN1)
import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.PubKey.RSA.Types (Error, PrivateKey, PublicKey (PublicKey))
import Crypto.Random (MonadRandom)
import Data.Aeson qualified as JSON (Value (Bool, Number, String), decodeStrict, encode)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap qualified as KM (KeyMap, empty, insert, lookup)
import Data.Aeson.Types (toJSON)
import Data.ByteArray (convert)
import Data.ByteString qualified as BSL (toStrict)
import Data.ByteString.Base64.URL qualified as B64URL (decodeUnpadded, encodeUnpadded)
import Data.ByteString.Char8 qualified as BS (ByteString, filter, head, intercalate, pack, reverse, split)
import Data.ByteString.Internal (unpackChars)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Debug.Trace
import Helpers

type JwtToken = BS.ByteString

type Claims = KM.KeyMap JSON.Value

newtype JwtM a = JwtM {runJwtM :: Claims -> (a, Claims)}

instance Functor JwtM where
  fmap :: (a -> b) -> JwtM a -> JwtM b
  fmap f (JwtM h) = JwtM $ \x -> let (b, claims) = h x in (f b, claims)

instance Applicative JwtM where
  pure x = JwtM (x,)
  (JwtM f) <*> (JwtM g) = JwtM $ \claims ->
    let (b, claims') = f claims
     in let (h, claims'') = g claims'
         in (b h, claims'')

instance Monad JwtM where
  (>>=) :: JwtM a -> (a -> JwtM b) -> JwtM b
  (JwtM f) >>= g = JwtM $ \claims ->
    let (a, claims') = f claims
     in runJwtM (g a) claims'

header :: BS.ByteString
header = B64URL.encodeUnpadded "{\"alg\":\"RS256\",\"typ\":\"JWT\"}"

seperator :: BS.ByteString
seperator = "."

hashAlg :: Maybe SHA256
hashAlg = Just SHA256

addClaim :: String -> JSON.Value -> JwtM ()
addClaim key value = JwtM $ \claims -> ((), KM.insert key' value claims)
  where
    key' = fromString key

addNumberClaim key = addClaim key . JSON.Number

addBoolClaim key = addClaim key . JSON.Bool

addStringClaim key = addClaim key . JSON.String . pack

getClaim :: String -> JwtM (Maybe JSON.Value)
getClaim key = JwtM $ \claims -> (KM.lookup key' claims, claims)
  where
    key' = fromString key

generateSignature :: (MonadRandom m) => PrivateKey -> BS.ByteString -> m (Either Error BS.ByteString)
generateSignature privateKey body = do
  eitherSignature <- RSA.signSafer hashAlg privateKey . BS.intercalate seperator $ [header, body]
  return $ fmap (B64URL.encodeUnpadded . convert) eitherSignature

splitJwt :: JwtToken -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
splitJwt jwt = case BS.split '.' jwt of
  [header, body, signature] -> return (header, body, signature)
  _ -> Nothing

splitAndDecodeJwt :: JwtToken -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
splitAndDecodeJwt jwt = case eitherToMaybe . traverse B64URL.decodeUnpadded . BS.split '.' $ jwt of
  Just [header', body, signature] -> Just (header', body, signature)
  _ -> Nothing

toToken :: (MonadRandom m) => PrivateKey -> JwtM a -> m (Either Error JwtToken)
toToken privateKey jwtMonad = do
  eitherSignature <- generateSignature privateKey body
  return $ fmap (BS.intercalate seperator . reverse . (: [body, header])) eitherSignature
  where
    body = B64URL.encodeUnpadded . BSL.toStrict . JSON.encode . snd . runJwtM jwtMonad $ KM.empty

fromToken :: PublicKey -> JwtToken -> Maybe Claims
fromToken publicKey token = do
  (header, body, _) <- splitJwt token
  (_, decodedBody, decodedSignature) <- splitAndDecodeJwt token
  let payload = header <> "." <> body
  if RSA.verify hashAlg publicKey payload decodedSignature
    then
      JSON.decodeStrict decodedBody
    else
      Nothing
