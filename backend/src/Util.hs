module Util
  ( maybeToEither
  , boolToEither
  , uuidToText
  , loadErrorCodes
  ) where

import Types (errorCodesFile, ErrorCodes(..))
import qualified Data.UUID as UUID
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x)  = Right x

boolToEither :: e -> Bool -> Either e ()
boolToEither _ True     = Right ()
boolToEither err False  = Left err

uuidToText :: UUID.UUID -> T.Text
uuidToText uuid =
  let bytes = LBS.toStrict $ encode uuid
      encoded = B64URL.encode bytes
  in T.replace (T.pack "=") (T.pack "") $ TE.decodeUtf8 encoded

loadErrorCodes :: IO ErrorCodes
loadErrorCodes = do
  json <- LBS.readFile errorCodesFile
  case A.eitherDecode json of
    Left err -> error $ "Failed to parse error constants: " ++ err
    Right cs -> return cs

