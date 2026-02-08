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
    _title <- obj .: "title"
    _link <- obj .: "link"
    _snippet <- obj .:? "snippet"
    return (Item {title=_title, link=_link, snippet=fromMaybe "" _snippet})
