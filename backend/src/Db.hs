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
import qualified Data.Text as T

import Types (User(..), UserId, EventId)
import qualified Types as E (Event(..))
import Types (TimeSlot, Error(..))
import Util (maybeToEither)

data DB = DB {
  _events :: Map.Map EventId E.Event
} deriving (Typeable)

$(deriveSafeCopy 0 'base ''DB)
$(makeLenses ''DB)

initialDBState :: DB
initialDBState = DB Map.empty

getEvent :: EventId -> Query DB (Maybe E.Event)
getEvent eventId = asks (Map.lookup eventId . _events)

insertNewEvent :: E.Event -> EventId -> Update DB ()
insertNewEvent event eventId = do
  events %= Map.insert eventId event

doAddAvailability :: UserId -> [TimeSlot] -> E.Event -> E.Event
doAddAvailability userId ts event = event {
  E.availabilities = foldr (\t acc ->
      Map.insertWith (++) t [userId] acc
    ) (E.availabilities event) ts
}

addAvailability :: EventId -> UserId -> T.Text -> [TimeSlot] -> Update DB Bool
addAvailability eventId userId userName ts = do
  mEvent <- use (events . at eventId)
  case mEvent of
    Just _ -> do
      events . at eventId . _Just %= (doAddUser userId . doAddAvailability userId ts)
      return True
    Nothing -> return False
  where
    doAddUser userId event = event {
      E.users = Map.insert userId User {
        Types.id = userId,
        name = userName
      } (E.users event)
    }

updateAvailability :: EventId -> UserId -> [TimeSlot] -> Update DB (Either Error ())
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
      return $ Right ()
  where
    clearAvailability event = event {
      E.availabilities = Map.map (filter (/= userId)) (E.availabilities event)
    }

$(makeAcidic ''DB ['insertNewEvent, 'getEvent, 'addAvailability, 'updateAvailability])

