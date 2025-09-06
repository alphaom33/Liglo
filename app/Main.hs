{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import Data.Text (Text)
import qualified Brick.BChan as B
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty (defaultConfig)
import Brick.BChan (readBChan, writeBChan)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Either (fromRight)
import Debug.Trace (trace)
import Control.Monad
import qualified Data.List as L

import qualified SearchApp as S
import qualified HTMLBuilder as H
import Message (Message(NewSearch, NextPage, LastPage))
import Network
import HTMLParser
import qualified CSSTokenizer as CT
import qualified CSSParser as CP

import Mortar
import StyleStealer

coolMain = do
    args <- getArgs
    let arged = map (\ c -> if c == ' ' then '+' else c) $ concat args
    key <- lookupEnv "GOOGLE_API_KEY"
    let apiKey = fromJust key
    finalState <- defaultMain S.app $ S.initialState apiKey $ head args

    when (S._curQuery finalState == head args) (do
        print "exited forcefully"
        exitSuccess)

    let finalURL = S._curQuery finalState
    writeFile "asdf.url" finalURL

    a <- parseRequest finalURL
    b <- httpLBS a
    let asdf = unpack (getResponseBody b)
    writeFile "asdf.html" asdf
    let result = _emitted $ parseString asdf

    let styleLinks = linkStyleTags $ grabStylesheets result
    yankedStyles <- foldr (\ a b -> (++) <$> nomUrl finalURL a <*> b) (pure []) styleLinks

    let styleInlines = takeStyles "" result

    let str = yankedStyles ++ styleInlines
    writeFile "asdf.css" str

    let out = CT.parseString str
    let outer = CP.parseList $ fromRight [] $ snd out
    let outest = H.parseWebpage result outer

    Mortar.appIt outest Mortar.initialState

testMain = do
    asdf <- readFile "asdf.html"
    let result = _emitted $ parseString asdf

    str <- readFile "asdf.css"

    let out = CT.parseString str
    let outer = CP.parseList $ fromRight [] $ snd out
    let outest = H.parseWebpage result outer

    -- putStr outest
    -- putStr $ H.drawCSSTree outer
    Mortar.appIt outest Mortar.initialState

main = testMain

