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
-- import Text.RegexPR as R
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

-- texxtify :: HTMLParser.Child -> String
-- texxtify (HTMLParser.ChildTag a) = textify a
-- texxtify (HTMLParser.ChildText a) = filter (/= '\n') a

-- textify :: HTMLParser.Tag -> String
-- textify (HTMLParser.Tag "head" children) = ""
-- textify (HTMLParser.Tag "p" children) = concatMap texxtify (reverse children) ++ "\n"
-- textify (HTMLParser.Tag "li" children) = concatMap texxtify (reverse children) ++ "\n"
-- textify (HTMLParser.Tag "tr" children) = concatMap texxtify (reverse children) ++ "\n"
-- textify (HTMLParser.Tag "th" children) = concatMap texxtify (reverse children) ++ " "
-- textify (HTMLParser.Tag "td" children)  = concatMap texxtify (reverse children) ++ " "
-- textify (HTMLParser.Tag _ children) = concatMap texxtify (reverse children)

main = do
    print $ parseString "<!doctype html SYSTEM 'asdf'><html><div a=asdf><div></div></div></html>"
    -- args <- getArgs 
    -- let arged = gsubRegexPR " " "+" $ concat args
    -- key <- lookupEnv "GOOGLE_API_KEY" 
    -- let apiKey = fromJust key 
    -- finalState <- defaultMain app $ initialState apiKey (head args) 

    -- a <- parseRequest $ _curQuery finalState
    -- b <- httpLBS a
    -- let asdf = unpack (getResponseBody b)

    -- let result = parseString asdf
    -- print result
    -- writeFile "asdf.html" $ textify $ fromRight HTMLParser.Tag {} $ snd $ result
