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
import Data.Text (Text, unpack)
import qualified Brick.BChan as B
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty (defaultConfig)
import Brick.BChan (readBChan, writeBChan)

import SearchApp
import Network
import Message (Message(NewSearch, NextPage, LastPage))
import HTMLParser (parseString)

main = do 
    print "start"
    print $ parseString "<!DOCTYPE html><a>a</a>"
    -- key <- lookupEnv "GOOGLE_API_KEY"
    -- let apiKey = fromJust key
    -- args <- getArgs
    -- finalState <- defaultMain app $ initialState apiKey (head args)
    -- print finalState
