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
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Status
import Data.Typeable
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Lens hiding ((.=))

-- Data Types
data User = User {
  id :: Int,
  name :: T.Text
} deriving (Show, Typeable, Generic)

instance A.ToJSON User
instance A.FromJSON User
$(deriveSafeCopy 0 'base ''User)

type TimeSlot = T.Text
type Availabilities = Map.Map TimeSlot [Int]

data UserAvailability = UserAvailability {
  userName :: T.Text,
  availability :: [TimeSlot]
} deriving (Show, Generic)
instance A.FromJSON UserAvailability
instance A.ToJSON UserAvailability

data Event = Event {
  name :: T.Text,
  users :: Map.Map Int User,
  startTime :: T.Text,
  endTime :: T.Text,
  availabilities :: Availabilities
} deriving (Show, Typeable, Generic)

instance A.ToJSON Event
instance A.FromJSON Event
$(deriveSafeCopy 0 'base ''Event)

-- State container
data EventDB = EventDB {
  _events :: Map.Map Int Event,
  _nextEventId :: Int,
  _nextUserId :: Int
} deriving (Typeable)

$(makeLenses ''EventDB)

-- Initial state
initialEventDBState :: EventDB
initialEventDBState = EventDB Map.empty 1 1

$(deriveSafeCopy 0 'base ''EventDB)

-- Acid-State queries and updates
getAllEvents :: Query EventDB [Event]
getAllEvents = asks (Map.elems . _events)

getEvent :: Int -> Query EventDB (Maybe Event)
getEvent eventId = asks (Map.lookup eventId . _events)

insertNewEvent :: Event -> Update EventDB Int
insertNewEvent event = do
  i <- use nextEventId
  events %= Map.insert i event
  nextEventId += 1
  return i

addAvailability :: Int -> T.Text -> [TimeSlot] -> Update EventDB Int
addAvailability eventId userName ts = do
  userId <- use nextUserId
  events %= \es -> case Map.lookup eventId es of
    Just event  -> Map.insert eventId (doAddAvailability event userId) es
    Nothing     -> es
  nextUserId += 1
  return userId
  where
    doAddAvailability event userId = event {
      availabilities = foldr (\t acc ->
        Map.insertWith (++) t [userId] acc
      ) (availabilities event) ts,
      users = Map.insert userId User {
        id = userId,
        name = userName
      } (users event)
    }

$(makeAcidic ''EventDB ['getAllEvents, 'insertNewEvent, 'getEvent, 'addAvailability])

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
      events' <- liftIO $ query acid GetAllEvents
      json events'

    get "/events/:eventId" $ do
      eventId <- pathParam "eventId"
      event <- liftIO $ query acid (GetEvent (read eventId :: Int))
      json event

    post "/events" $ do
      event <- jsonData
      eventId <- liftIO $ update acid (InsertNewEvent event)
      status status201
      json $ A.object ["eventId" A..= eventId]

    post "/events/:eventId/availability" $ do
      eventId <- pathParam "eventId"
      userAvailability <- jsonData :: ActionM UserAvailability
      let uname = userName userAvailability
      let avail = availability userAvailability
      userId <- liftIO $ update acid (AddAvailability (read eventId :: Int) uname avail)
      status status201
      json $ A.object ["userId" A..= userId]

    notFound $ do
        status status404
        json $ A.object ["error" A..= ("Route not found" :: String)]

  -- Close Acid-State on shutdown
  closeAcidState acid
