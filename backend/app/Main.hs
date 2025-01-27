{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Web.Scotty
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as A
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Status
import GHC.Generics
import qualified Data.Text as T
import Control.Exception (SomeException)

-- Data type for items
data Item = Item {
    name :: T.Text,
    description :: T.Text
} deriving (Show, Generic)

instance A.ToJSON Item
instance A.FromJSON Item

-- MongoDB configuration
type MongoAction = Action IO

runMongo :: MongoAction a -> IO a
runMongo action = do
    pipe <- connect (host "127.0.0.1")
    access pipe master "test" action

-- Database operations
getItems :: MongoAction [Item]
getItems = do
    docs <- find (select [] "items") >>= rest
    return $ map (\ doc -> Item 
        { name = at "name" doc
        , description = at "description" doc 
        }) docs

insertItem :: Item -> MongoAction A.Value
insertItem item = do
    _ <- insert "items" [ "name" =: name item
                       , "description" =: description item
                       ]
    return $ A.Object mempty

-- Main application
main :: IO ()
main = scotty 3000 $ do
    -- Middleware
    middleware simpleCors
    middleware logStdoutDev

    -- Routes
    get "/items" $ do
        items <- liftAndCatchIO $ runMongo getItems
        json items
    
    post "/items" $ do
        item <- jsonData
        result <- liftAndCatchIO $ runMongo (insertItem item)
        status status201
        json result
    
    notFound $ do
        status status404
        json $ A.object ["error" A..= ("Route not found" :: String)]

    -- Error handling
    -- defaultHandler $ do
    --     status status500
