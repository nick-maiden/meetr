module Util
  ( maybeToEither,
    uuidToText
  ) where

import qualified Data.UUID as UUID
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

uuidToText :: UUID.UUID -> T.Text
uuidToText uuid =
  let bytes = LBS.toStrict $ encode uuid
      encoded = B64URL.encode bytes
  in T.replace (T.pack "=") (T.pack "") $ TE.decodeUtf8 encoded

