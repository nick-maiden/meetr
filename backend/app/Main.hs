{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Types
  ( UserAvailability(..)
  , UpdateAvailabilityRequest(..)

  , Error(..)
  , eventToEventResponse
  )
import Db
  ( initialDBState
  , GetEvent(..)
  , InsertNewEvent(..)
  , AddAvailability(..)
  , UpdateAvailability(..)
  )
import Util (uuidToText)
import Web.Scotty
import Data.Acid
import qualified Data.Aeson as A
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import qualified Data.Text as Text
import qualified Data.UUID.V4 as UUID4

main :: IO ()
main = do
  acid <- openLocalState initialDBState
  scottyOpts (Options 1 (setHost "*6" $ setPort 8080 defaultSettings)) $ do

    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE"]
      , corsOrigins = Just (["http://localhost:5173"], True)
      }
    middleware logStdoutDev

    -- routes
    get "/events/:eventId" $ do
      eventId <- pathParam "eventId"
      event <- liftIO $ query acid (GetEvent eventId)
      case event of
        Just e -> json $ eventToEventResponse eventId e
        Nothing -> status status404 >> json ("Event not found" :: Text.Text)

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
      eventFound <- liftIO $ update acid (AddAvailability eventId userId name availability)
      case eventFound of
        True  -> status status201 >> json userId
        False -> status status404 >> json ("Event not found" :: Text.Text)

    put "/events/:eventId/availability/:userId" $ do
      eventId <- pathParam "eventId"
      userId <- pathParam "userId"
      UpdateAvailabilityRequest{..} <- jsonData
      result <- liftIO $ update acid (UpdateAvailability eventId userId availability)
      case result of
        Left EventNotFound -> status status404 >> json ("Event not found" :: Text.Text)
        Left UserNotFound -> status status404 >> json ("User not found" :: Text.Text)
        Right () -> status status204

    notFound $ do
        status status404
        json $ A.object ["error" A..= ("Route not found" :: String)]

  closeAcidState acid

