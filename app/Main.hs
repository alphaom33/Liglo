{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Webpage
import qualified SearchApp
import qualified HTMLParser

import Data.Tree
import System.Exit
import System.Environment (getArgs, lookupEnv)
import Brick as B
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
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
import Data.Either (fromRight)
import Debug.Trace (trace)
import Control.Monad

import qualified SearchApp as S
import qualified HTMLBuilder as H
import Message (Message(NewSearch, NextPage, LastPage))
import Network
import HTMLParser
import qualified CSSTokenizer as CT
import qualified CSSParser as CP

main = do
    -- args <- getArgs
    -- let arged = map (\ c -> if c == ' ' then '+' else c) $ concat args
    -- key <- lookupEnv "GOOGLE_API_KEY"
    -- let apiKey = fromJust key
    -- finalState <- defaultMain S.app $ S.initialState apiKey (args!!0)
    --
    -- when (S._curQuery finalState == args!!0) (do
    --     print "exited forcefully"
    --     exitSuccess)
 

    -- a <- parseRequest $ "https://hoogle.haskell.org?hoogle=map"--S._curQuery finalState
    -- b <- httpLBS a
    -- let asdf = unpack (getResponseBody b)
    asdf <- readFile "asdf.html"
    let result = parseString asdf

    str <- readFile "asdf.css"
    let out = CT.parseString $ str
    let outer = CP.parseList $ fromRight [] $ snd out

    defaultMain H.app $ H.initialState outer $ _emitted result
