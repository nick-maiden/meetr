module Config
  ( defaultConfig
  , readConfig
  ) where

import Types (Config(..), LogLevel(..))
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

defaultConfig :: Config
defaultConfig = Config
  { port = 8080
  , host = "127.0.0.1"
  , allowedOrigins = ["http://127.0.0.1:5173"]
  , logLevel = Production
  }

-- Read configuration from environment variables
readConfig :: IO Config
readConfig = do
  port <- lookupEnvInt "PORT" (port defaultConfig)
  host <- lookupEnvString "HOST" (host defaultConfig)
  origins <- lookupEnvStringList "ALLOWED_ORIGINS" (allowedOrigins defaultConfig)
  logLevel <- lookupEnvEnum "LOG_LEVEL" (logLevel defaultConfig)

  return Config
    { port = port
    , host = host
    , allowedOrigins = origins
    , logLevel = logLevel
    }
  where
    lookupEnvInt name defaultVal = do
      val <- lookupEnv name
      return $ fromMaybe defaultVal (val >>= readMaybe)

    lookupEnvString name defaultVal = do
      val <- lookupEnv name
      return $ fromMaybe defaultVal val

    lookupEnvStringList name defaultVal = do
      val <- lookupEnv name
      return $ maybe defaultVal (splitOn ',') val

    lookupEnvEnum name defaultVal = do
      val <- lookupEnv name
      return $ fromMaybe defaultVal (val >>= readMaybe)

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
      (x, []) -> [x]
      (x, _:xs) -> x : splitOn c xs

