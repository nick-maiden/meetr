{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Db
  ( DB(..)
  , initialDBState
  , GetEvent(..)
  , InsertNewEvent(..)
  , AddAvailability(..)
  , UpdateAvailability(..)
  ) where

import qualified Data.Map as Map
import Data.Typeable
import Data.SafeCopy
import Control.Lens
import Control.Monad.Reader
import Data.Acid
import qualified Data.Text as Text

import Types (User(..))
import qualified Types as E (Event(..))
import Types (TimeSlot, Error(..))
import Util (maybeToEither)

data DB = DB
  { _events      :: Map.Map Int E.Event
  , _nextEventId :: Int
  , _nextUserId  :: Int
  } deriving (Typeable)

$(deriveSafeCopy 0 'base ''DB)
$(makeLenses ''DB)

initialDBState :: DB
initialDBState = DB Map.empty 1 1

getEvent :: Int -> Query DB (Maybe E.Event)
getEvent eventId = asks (Map.lookup eventId . _events)

insertNewEvent :: E.Event -> Update DB Int
insertNewEvent event = do
  i <- use nextEventId
  events %= Map.insert i event
  nextEventId += 1
  return i

doAddAvailability :: Int -> [TimeSlot] -> E.Event -> E.Event
doAddAvailability userId ts event = event {
  E.availabilities = foldr (\t acc ->
      Map.insertWith (++) t [userId] acc
    ) (E.availabilities event) ts
}

addAvailability :: Int -> Text.Text -> [TimeSlot] -> Update DB (Maybe Int)
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
      E.users = Map.insert userId User {
        Types.id = userId,
        name = userName
      } (E.users event)
    }

updateAvailability :: Int -> Int -> [TimeSlot] -> Update DB (Either Error ())
updateAvailability eventId userId ts = do
  mEvent <- use (events . at eventId)
  case do
    event <- maybeToEither EventNotFound mEvent
    _     <- maybeToEither UserNotFound (Map.lookup userId (E.users event))
    return $ Right ()
   of
    Left err -> return $ Left err
    Right _ -> do
      events . at eventId . _Just %= (doAddAvailability userId ts . clearAvailability)
      nextUserId += 1
      return $ Right ()
  where
    clearAvailability event = event {
      E.availabilities = Map.map (filter (/= userId)) (E.availabilities event)
    }

$(makeAcidic ''DB ['insertNewEvent, 'getEvent, 'addAvailability, 'updateAvailability])
