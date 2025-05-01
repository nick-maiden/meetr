{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( User(..)
  , UserId
  , Config(..)
  , LogLevel(..)
  , EventId
  , TimeSlot
  , Event(..)
  , UserAvailability(..)
  , UpdateAvailabilityRequest(..)
  , EventResponse(..)
  , Error(..)
  , Availabilities
  , eventToEventResponse
  ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable
import GHC.Generics
import Data.SafeCopy
import qualified Data.Aeson as A

-- -----------------------------
-- Core domain types
-- -----------------------------

type UserId = T.Text
type EventId = T.Text

data User = User
  { id   :: UserId
  , name :: T.Text
  } deriving (Show, Typeable, Generic)

type Users = Map.Map UserId User
type TimeSlot = T.Text
type Availabilities = Map.Map TimeSlot [UserId]

data Event = Event
  { name            :: T.Text
  , users           :: Users
  , dates           :: [T.Text]
  , earliestTime    :: T.Text
  , latestTime      :: T.Text
  , availabilities  :: Availabilities
  } deriving (Show, Typeable, Generic)

-- -----------------------------
-- Server configuration types
-- -----------------------------

data Config = Config
  { port :: Int
  , host :: String
  , allowedOrigins :: [String]
  , logLevel :: LogLevel
  }

data LogLevel = Development | Production
  deriving (Show, Read, Eq)

-- -----------------------------
-- API Request/Response types
-- -----------------------------

data UserAvailability = UserAvailability
  { name     :: T.Text
  , availability :: [TimeSlot]
  } deriving (Show, Generic)

data UpdateAvailabilityRequest = UpdateAvailabilityRequest {
  availability :: [TimeSlot]
} deriving (Show, Generic)

data EventResponse = EventResponse
  { id              :: EventId
  , name            :: T.Text
  , users           :: Users
  , dates           :: [T.Text]
  , earliestTime    :: T.Text
  , latestTime      :: T.Text
  , availabilities  :: Availabilities
  } deriving (Show, Generic)

-- -----------------------------
-- Error types
-- -----------------------------

data Error
  = EventNotFound
  | UserNotFound
  deriving (Show)

-- -----------------------------
-- JSON instances
-- -----------------------------

instance A.ToJSON User
instance A.FromJSON User
instance A.ToJSON Event
instance A.FromJSON Event
instance A.ToJSON EventResponse
instance A.FromJSON EventResponse
instance A.FromJSON UserAvailability
instance A.ToJSON UserAvailability
instance A.FromJSON UpdateAvailabilityRequest

-- -----------------------------
-- SafeCopy instances
-- -----------------------------

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Event)
$(deriveSafeCopy 0 'base ''Error)

-- -----------------------------
-- Conversion functions
-- -----------------------------

eventToEventResponse :: EventId -> Event -> EventResponse
eventToEventResponse eventId Event{..} = EventResponse
  { id              = eventId
  , name            = name
  , users           = users
  , dates           = dates
  , earliestTime    = earliestTime
  , latestTime      = latestTime
  , availabilities  = availabilities
  }

