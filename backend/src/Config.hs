module Config
  (readConfig) where

import Types (Config(..))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import System.Exit (die)

-- Read configuration from environment variables
readConfig :: IO Config
readConfig = do
  portStr <- lookupEnv "PORT" >>= maybe (die "PORT env var required") return
  hostStr <- lookupEnv "HOST" >>= maybe (die "HOST env var required") return
  originsStr <- lookupEnv "ALLOWED_ORIGINS" >>= maybe (die "ALLOWED_ORIGINS env var required") return
  logLevelStr <- lookupEnv "LOG_LEVEL" >>= maybe (die "LOG_LEVEL env var required") return

  port <- case readMaybe portStr of
            Just p  -> return p
            Nothing -> die "PORT must be a valid integer"

  let origins = splitOn ',' originsStr

  logLevel <- case readMaybe logLevelStr of
               Just l  -> return l
               Nothing -> die "LOG_LEVEL must be a valid enum value"

  return Config
    { port = port
    , host = hostStr
    , allowedOrigins = origins
    , logLevel = logLevel
    }
  where
    splitOn c s = case break (== c) s of
      (x, []) -> [x]
      (x, _:xs) -> x : splitOn c xs

