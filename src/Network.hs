{-# LANGUAGE DeriveGeneric #-}
module Network where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
import Item (Item)

newtype GoogleResponse = GoogleResponse {
    items :: [Item]
} deriving (Show, Generic)

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

getResponse :: String -> String -> Int -> IO [Item]
getResponse initial key start = do
    let query = initial
        requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb" ++ "&start=" ++ show (start * 10)
    request <- S.parseRequest requestScheme
    let response = S.httpJSON request
    let googleResponse = fmap getResponseBody response
        itemResponse = fmap items googleResponse

    itemResponse
