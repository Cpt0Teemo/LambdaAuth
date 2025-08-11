{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Crypto.Number.Serialize (i2osp)
import Crypto.PubKey.Curve25519 ()
import Crypto.PubKey.RSA (PublicKey (..))
import Crypto.PubKey.RSA.Types
import Crypto.PubKey.RSA.Types (PrivateKey, PublicKey (PublicKey))
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Char8 qualified as BS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE (decodeUtf8, encodeUtf8)
import Data.X509 (Certificate (certPubKey), PubKey (PubKeyRSA), getSigned, signedObject)
import Data.X509 qualified as X509
import Data.X509.File (readKeyFile, readSignedObject)
import Data.X509.Memory (readKeyFileFromMemory)

bsToLazyText :: BS.ByteString -> TL.Text
bsToLazyText = TLE.decodeUtf8 . BS.fromStrict

lazyTextToBS :: TL.Text -> BS.ByteString
lazyTextToBS = BS.toStrict . TLE.encodeUtf8

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

readPrivateKeyFromPem :: FilePath -> IO (Either String PrivateKey)
readPrivateKeyFromPem filePath = do
  pemContent <- readKeyFile filePath
  case pemContent of
    [X509.PrivKeyRSA privateKey] -> return $ Right privateKey
    _ -> return $ Left "No keys found in PEM file"

retrievePublicKey :: PrivateKey -> PublicKey
retrievePublicKey = toPublicKey . KeyPair

publicKeyToJWK :: PublicKey -> Value
publicKeyToJWK pubKey =
  object
    [ "kty" .= ("RSA" :: String),
      "use" .= ("sig" :: String),
      "kid" .= ("LambdaAuthId" :: String),
      "alg" .= ("RS256" :: String),
      "n" .= BS.unpack (B64URL.encodeUnpadded (i2osp (public_n pubKey))),
      "e" .= BS.unpack (B64URL.encodeUnpadded (i2osp (public_e pubKey)))
    ]
