{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Types
  ( UserAvailability(..)
  , UpdateAvailabilityRequest(..)
  , Config(..)
  , LogLevel(..)
  , BaseErrors(..)
  , AddAvailabilityError(..)
  , UpdateAvailabilityError(..)
  , ErrorCodes(..)
  , eventToEventResponse
  )
import Db
  ( initialDBState
  , GetEvent(..)
  , InsertNewEvent(..)
  , AddAvailability(..)
  , UpdateAvailability(..)
  )
import Util (uuidToText, loadErrorCodes)
import Config (readConfig)
import Web.Scotty
import Data.Acid
import qualified Data.Aeson as A
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID.V4 as UUID4
import Data.String (fromString)

main :: IO ()
main = do
  errorCodes <-loadErrorCodes
  config <- readConfig
  acid <- openLocalState initialDBState

  let settings = setHost (fromString $ host config) $
                 setPort (port config) defaultSettings

  let logging = case logLevel config of
        Development -> logStdoutDev
        Production  -> logStdout

  scottyOpts (Options 1 settings) $ do
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE"]
      , corsOrigins = Just (map BS.pack (allowedOrigins config), True)
      }
    middleware logging

    -- routes
    get "/events/:eventId" $ do
      eventId <- pathParam "eventId"
      event <- liftIO $ query acid (GetEvent eventId)
      case event of
        Just e -> json $ eventToEventResponse eventId e
        Nothing -> status status404 >> json (errEventNotFound errorCodes)

    post "/events" $ do
      event <- jsonData
      uuid <- liftIO UUID4.nextRandom
      let eventId = uuidToText uuid
      liftIO $ update acid (InsertNewEvent event eventId)
      status status201
      json $ A.object ["eventId" A..= eventId]

    post "/events/:eventId/availability" $ do
      eventId <- pathParam "eventId"
      UserAvailability{..} <- jsonData
      uuid <- liftIO UUID4.nextRandom
      let userId = uuidToText uuid
      result <- liftIO $ update acid (AddAvailability eventId userId name availability)
      case result of
        Left (AddAvailabilityCommon EventNotFound)  -> status status404 >> json (errEventNotFound errorCodes)
        Left UsernameTaken                          -> status status404 >> json (errUsernameTaken errorCodes)
        Right ()                                    -> status status201 >> json userId

    put "/events/:eventId/availability/:userId" $ do
      eventId <- pathParam "eventId"
      userId <- pathParam "userId"
      UpdateAvailabilityRequest{..} <- jsonData
      result <- liftIO $ update acid (UpdateAvailability eventId userId availability)
      case result of
        Left (UpdateAvailabilityCommon EventNotFound) -> status status404 >> json (errEventNotFound errorCodes)
        Left UserNotFound                             -> status status404 >> json (errUserNotFound errorCodes)
        Right ()                                      -> status status204

    notFound $ do
        status status404
        json $ A.object ["error" A..= ("Route not found" :: String)]

  closeAcidState acid

