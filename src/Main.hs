{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Webpage
import qualified SearchApp
import qualified HTMLParser

import System.Environment (getArgs, lookupEnv)
import Brick as B
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
import Text.RegexPR as R
import Data.Maybe (fromJust)
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form(formFocus, formState), handleFormEvent)
import Lens.Micro.Extras (view)
import Control.Monad (join)
import Data.Text (Text)
import qualified Brick.BChan as B
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty (defaultConfig)
import Brick.BChan (readBChan, writeBChan)
import Data.ByteString.Lazy.Char8 (unpack)

import SearchApp
import Network
import Message (Message(NewSearch, NextPage, LastPage))
import HTMLParser (parseString)
import Data.Either (fromRight)

main = do
    a <- parseRequest "https://hackage.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Char8.html#v:unpack"
    b <- httpLBS a
    let asdf = unpack (getResponseBody b)
    writeFile "asdf.html" $ show $ fromRight HTMLParser.Tag {} $ snd $ parseString asdf





    -- key <- lookupEnv "GOOGLE_API_KEY" let apiKey = fromJust key args <- getArgs finalState <- defaultMain app $ initialState apiKey (head args) print finalState
