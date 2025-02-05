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

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

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
  earliestTime :: T.Text,
  latestTime :: T.Text,
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

doAddAvailability :: Int -> [TimeSlot] -> Event -> Event
doAddAvailability userId ts event = event {
  availabilities = foldr (\t acc ->
      Map.insertWith (++) t [userId] acc
    ) (availabilities event) ts,
  users = (users event)
}

addAvailability :: Int -> T.Text -> [TimeSlot] -> Update EventDB (Maybe Int)
addAvailability eventId userName ts = do
  userId <- use nextUserId
  mEvent <- use (events . at eventId)
  case mEvent of
    Just _ -> do
      events . at eventId . _Just %= (doAddUser userId . doAddAvailability userId ts)
      nextUserId += 1
      return $ Just userId
    Nothing -> return Nothing
  where
    doAddUser userId event = event {
      availabilities = (availabilities event),
      users = Map.insert userId User {
        id = userId,
        name = userName
      } (users event)
    }

data UpdateAvailabilityError =
  EventNotFound
  | UserNotFound
  deriving (Show)

$(deriveSafeCopy 0 'base ''UpdateAvailabilityError)

updateAvailability :: Int -> Int -> [TimeSlot] -> Update EventDB (Either UpdateAvailabilityError ())
updateAvailability eventId userId ts = do
  mEvent  <- use (events . at eventId)
  case do
    event <- maybeToEither EventNotFound mEvent
    _     <- maybeToEither UserNotFound (Map.lookup userId (users event))
    return $ Right ()
   of
    Left err  -> return $ Left err
    Right _   -> do
      events . at eventId . _Just %= (doAddAvailability userId ts . clearAvailability)
      nextUserId += 1
      return $ Right ()
  where
    clearAvailability event = event {
      availabilities = Map.map (filter (/= userId)) (availabilities event),
      users = users event
    }

$(makeAcidic ''EventDB ['getAllEvents, 'insertNewEvent, 'getEvent, 'addAvailability, 'updateAvailability])

main :: IO ()
main = do
  acid <- openLocalState initialEventDBState

  scotty 8080 $ do
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE"]
      , corsOrigins = Just (["http://localhost:5173"], True)
      }
    middleware logStdoutDev

    -- routes
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
      result <- liftIO $ update acid (AddAvailability (read eventId :: Int) uname avail)
      case result of
        Just userId -> status status201 >> json userId
        Nothing -> status status404 >> json ("Event not found" :: T.Text)

    put "/events/:eventId/availability/:userId" $ do
      eventId <- pathParam "eventId"
      userId <- pathParam "userId"
      avail <- jsonData
      result <- liftIO $ update acid (UpdateAvailability (read eventId :: Int) (read userId :: Int) avail)
      case result of
        Left EventNotFound -> status status404 >> json ("Event not found" :: T.Text)
        Left UserNotFound -> status status404 >> json ("User not found" :: T.Text)
        Right () -> status status204

    notFound $ do
        status status404
        json $ A.object ["error" A..= ("Route not found" :: String)]

  closeAcidState acid
