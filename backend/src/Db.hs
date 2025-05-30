{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Db
  ( DB(..)
  , initialDBState
  , GetEvent(..)
  , CreateEvent(..)
  , AddAvailability(..)
  , UpdateAvailability(..)
  ) where

import qualified Data.Map as Map
import Data.Typeable
import Data.SafeCopy
import Control.Lens
import Control.Monad.Reader
import Data.Acid
import qualified Data.Text as T

import Types
  ( User(..)
  , BaseErrors(..)
  , AddAvailabilityError(..)
  , UpdateAvailabilityError(..)
  , UserId
  , EventId
  , EventInput(..)
  , Event(..)
  , TimeSlot
  )
import Util (maybeToEither, boolToEither)

data DB = DB {
  _events :: Map.Map EventId Event
} deriving (Typeable)

$(deriveSafeCopy 0 'base ''DB)
$(makeLenses ''DB)

initialDBState :: DB
initialDBState = DB Map.empty

getEvent :: EventId -> Query DB (Maybe Event)
getEvent eventId = asks (Map.lookup eventId . _events)

createEvent :: EventInput -> EventId -> Update DB ()
createEvent eventInput eventId = do
  let event = Event {
    name = eventInput.name,
    users = mempty,
    dates = eventInput.dates,
    earliestTime = eventInput.earliestTime,
    latestTime = eventInput.latestTime,
    availabilities = mempty
  }
  events %= Map.insert eventId event

doAddAvailability :: UserId -> [TimeSlot] -> Event -> Event
doAddAvailability userId ts event = event {
  availabilities = foldr (\t acc ->
      Map.insertWith (++) t [userId] acc
    ) event.availabilities ts
}

addAvailability :: EventId -> UserId -> T.Text -> [TimeSlot] -> Update DB (Either AddAvailabilityError ())
addAvailability eventId userId userName ts = do
  mEvent <- use (events . at eventId)
  case do
    event <- maybeToEither (AddAvailabilityCommon EventNotFound) mEvent
    _     <- boolToEither UsernameTaken $ not $ any (\u -> u.name == userName) (Map.elems event.users)
    return $ Right ()
   of
    Left err  -> return $ Left err
    Right _   -> do
      events . at eventId . _Just %= (doAddUser userId . doAddAvailability userId ts)
      return $ Right ()
  where
    doAddUser userId event = event {
      users = Map.insert userId User {
        id = userId,
        name = userName
      } event.users
    }

updateAvailability :: EventId -> UserId -> [TimeSlot] -> Update DB (Either UpdateAvailabilityError ())
updateAvailability eventId userId ts = do
  mEvent <- use (events . at eventId)
  case do
    event <- maybeToEither (UpdateAvailabilityCommon EventNotFound) mEvent
    _     <- maybeToEither UserNotFound (Map.lookup userId event.users)
    return $ Right ()
   of
    Left err  -> return $ Left err
    Right _   -> do
      events . at eventId . _Just %= (doAddAvailability userId ts . clearAvailability)
      return $ Right ()
  where
    clearAvailability event = event {
      availabilities = Map.map (filter (/= userId)) event.availabilities
    }

$(makeAcidic ''DB ['createEvent, 'getEvent, 'addAvailability, 'updateAvailability])

