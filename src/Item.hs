{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Item where

import Data.Maybe
import GHC.Generics
import Data.Aeson

data Item = Item {
    title :: String
    , link :: String
    , snippet :: String
} deriving (Show, Generic)

instance ToJSON Item
instance FromJSON Item where
  parseJSON = withObject "Item" $ \ obj -> do
    title <- obj .: "title"
    link <- obj .: "link"
    snippet <- obj .:? "snippet"
    return (Item {title=title, link=link, snippet=fromMaybe "" snippet})
