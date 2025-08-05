{-# LANGUAGE OverloadedStrings #-}
module JWT (addStringClaim, addBoolClaim, addNumberClaim) where

import qualified Data.Aeson.KeyMap as KM ( insert, KeyMap, lookup, empty )
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (toJSON)
import Data.Aeson (encode, Value, Value(Number), Value(String), Value(Bool))
import qualified Data.ByteString.Char8 as BS (ByteString, pack, intercalate, filter, split, head, reverse)
import Data.ByteString.Internal (unpackChars)
import Crypto.Hash (SHA256, hash)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import qualified Data.ByteString as BSL (toStrict)
import Data.ByteArray (convert)
import qualified Data.ByteString.Base64.URL as B64URL (encodeUnpadded)
import Data.Text (pack)
import Data.Maybe (fromMaybe)

type JwtToken = BS.ByteString
type JwtSecret = BS.ByteString
type Claims = KM.KeyMap Value
newtype JwtM a = JwtM { runJwtM :: Claims -> (a, Claims) }

instance Functor JwtM where
    fmap :: (a -> b) -> JwtM a -> JwtM b
    fmap f (JwtM h) = JwtM $ \x -> let (b, claims) = h x in (f b, claims)

instance Applicative JwtM where
    pure x = JwtM (x, )
    (JwtM f) <*> (JwtM g) = JwtM $ \claims ->
        let (b, claims') = f claims in
        let (h, claims'') = g claims' in
        (b h, claims'')

instance Monad JwtM where
    (>>=) :: JwtM a -> (a -> JwtM b) -> JwtM b
    (JwtM f) >>= g = JwtM $ \claims ->
        let (a, claims') = f claims in
        runJwtM (g a) claims'

header :: BS.ByteString
header = B64URL.encodeUnpadded "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"

seperator :: BS.ByteString
seperator = "."

addClaim :: String -> Value -> JwtM ()
addClaim key value = JwtM $ \claims -> ((), KM.insert key' value claims)
    where
        key' = fromString key

addNumberClaim key = addClaim key . Number  
addBoolClaim key = addClaim key . Bool
addStringClaim key = addClaim key . String . pack

getClaim :: String -> JwtM (Maybe Value)
getClaim key = JwtM $ \claims -> (KM.lookup key' claims, claims)
    where
        key' = fromString key
        
generateSignature :: JwtSecret -> BS.ByteString -> BS.ByteString
generateSignature secret body = B64URL.encodeUnpadded . convert . hmacGetDigest . generateHMAC secret . BS.intercalate seperator $ [header, body]

splitJwt :: JwtToken -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
splitJwt jwt = case BS.split '.' jwt of
    [header', body, signature] -> Just (header', body, signature)
    _                         -> Nothing

isValid :: BS.ByteString -> BS.ByteString -> Bool
isValid secret = maybe False (\(_, body, signature) -> expectedSignature body == signature ) . splitJwt
    where
        expectedSignature = generateSignature secret

toToken :: JwtSecret -> JwtM a -> JwtToken
toToken secret jwtMonad = BS.intercalate seperator [header, body, signature]
    where
        signature = generateSignature secret body
        body = B64URL.encodeUnpadded . BSL.toStrict . encode . snd . runJwtM jwtMonad $ KM.empty

fromToken :: JwtToken -> Maybe (JwtM ())
fromToken = undefined

generateHMAC :: BS.ByteString -> BS.ByteString -> HMAC SHA256
generateHMAC = hmac