{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Web.Scotty
import Data.Acid
import Data.SafeCopy
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Status
import Data.Typeable
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Lens hiding ((.=))
import Data.Text.Lazy (pack)

-- Data Types
data User = User {
  id :: Int,
  name :: T.Text
} deriving (Show, Typeable, Generic)

instance A.ToJSON User
instance A.FromJSON User
$(deriveSafeCopy 0 'base ''User)

data Event = Event {
  name :: T.Text,
  users :: [User],
  startTime :: T.Text,
  endTime :: T.Text
} deriving (Show, Typeable, Generic)

instance A.ToJSON Event
instance A.FromJSON Event
$(deriveSafeCopy 0 'base ''Event)

-- State container
data EventDB = EventDB {
    _events :: Map.Map Int Event,
    _nextId :: Int
} deriving (Typeable)

$(makeLenses ''EventDB)

-- Initial state
initialEventDBState :: EventDB
initialEventDBState = EventDB Map.empty 0

$(deriveSafeCopy 0 'base ''EventDB)

-- Acid-State queries and updates
getAllEvents :: Query EventDB [Event]
getAllEvents = asks (Map.elems . _events)

getEvent :: Int -> Query EventDB (Maybe Event)
getEvent eventId = do
  db <- ask
  return (Map.lookup eventId (_events db))

insertNewEvent :: Event -> Update EventDB Int
insertNewEvent event = do
    i <- use nextId
    events %= Map.insert i event
    nextId += 1
    return i

$(makeAcidic ''EventDB ['getAllEvents, 'insertNewEvent, 'getEvent])

-- Main application
main :: IO ()
main = do
    -- Initialize Acid-State
    acid <- openLocalState initialEventDBState

    -- Start Scotty
    scotty 3000 $ do
        -- Middleware
        middleware simpleCors
        middleware logStdoutDev

        -- Routes
        get "/events" $ do
            events <- liftAndCatchIO $ query acid GetAllEvents
            json events

        get "/events/:id" $ do
          eventId <- param "id"
          event <- liftAndCatchIO $ query acid (GetEvent (read eventId :: Int))
          json event

        post "/events" $ do
            event <- jsonData
            eventId <- liftAndCatchIO $ update acid (InsertNewEvent event)
            status status201
            json $ A.object ["id" A..= eventId]

        notFound $ do
            status status404
            json $ A.object ["error" A..= ("Route not found" :: String)]

        -- Error handling
        -- defaultHandler $ \err -> do
        --     status status500
        --     json $ A.object ["error" A..= pack (show err)]

    -- Close Acid-State on shutdown
    closeAcidState acid
