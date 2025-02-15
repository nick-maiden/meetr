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
import Web.Scotty
import Data.Acid
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Status
import qualified Data.Text as Text

main :: IO ()
main = do
  acid <- openLocalState initialDBState
  scotty 8080 $ do

    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE"]
      , corsOrigins = Just (["http://localhost:5173"], True)
      }
    middleware logStdoutDev

    -- routes
    get "/events/:eventId" $ do
      eventId <- pathParam "eventId"
      event <- liftIO $ query acid (GetEvent (read eventId))
      case event of
        Just e -> json $ eventToEventResponse (read eventId) e
        Nothing -> status status404 >> json ("Event not found" :: Text.Text)

    post "/events" $ do
      event <- jsonData
      eventId <- liftIO $ update acid (InsertNewEvent event)
      status status201
      json $ A.object ["eventId" A..= eventId]

    post "/events/:eventId/availability" $ do
      eventId <- pathParam "eventId"
      UserAvailability{..} <- jsonData
      result <- liftIO $ update acid (AddAvailability (read eventId) name availability)
      case result of
        Just userId -> status status201 >> json userId
        Nothing -> status status404 >> json ("Event not found" :: Text.Text)

    put "/events/:eventId/availability/:userId" $ do
      eventId <- pathParam "eventId"
      userId <- pathParam "userId"
      UpdateAvailabilityRequest{..} <- jsonData
      result <- liftIO $ update acid (UpdateAvailability (read eventId) (read userId) availability)
      case result of
        Left EventNotFound -> status status404 >> json ("Event not found" :: Text.Text)
        Left UserNotFound -> status status404 >> json ("User not found" :: Text.Text)
        Right () -> status status204

    notFound $ do
        status status404
        json $ A.object ["error" A..= ("Route not found" :: String)]

  closeAcidState acid

