{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit
import System.Environment (getArgs, lookupEnv)
import qualified Brick
import Network.HTTP.Simple as S
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Either (fromRight)
import Control.Monad

import qualified SearchApp
import qualified HTMLBuilder
import HTMLParser
import qualified CSSTokenizer
import qualified CSSParser
import Mortar
import StyleStealer

searchMain :: IO ()
searchMain = do
    args <- getArgs
    let arged = map (\ c -> if c == ' ' then '+' else c) $ concat args
    when (null arged) (do
        print ("Failure: no search supplied" :: String)
        exitFailure)

    key <- lookupEnv "GOOGLE_API_KEY"
    let apiKey = fromJust key
    finalState <- Brick.defaultMain SearchApp.app $ SearchApp.initialState apiKey $ head args

    when (SearchApp._curQuery finalState == head args) (do
        exitSuccess)

    let finalURL = SearchApp._curQuery finalState
    writeFile "test.url" finalURL

    request <- parseRequest finalURL
    response <- httpLBS request
    let html = unpack (getResponseBody response)
    writeFile "test.html" html
    let result = _emitted $ parseString html

    let styleLinks = linkStyleTags $ grabStylesheets result
    yankedStyles <- foldr (\ a b -> (++) <$> nomUrl finalURL a <*> b) (pure []) styleLinks

    let styleInlines = takeStyles "" result

    let str = yankedStyles ++ styleInlines
    writeFile "test.css" str

    let out = CSSTokenizer.parseString str
    let outer = CSSParser.parseList $ fromRight [] $ snd out
    let outest = HTMLBuilder.parseWebpage result outer

    Mortar.appIt outest Mortar.initialState

testMain :: IO ()
testMain = do
    html <- readFile "test.html"
    let result = _emitted $ parseString html

    str <- readFile "test.css"

    let out = CSSTokenizer.parseString str
    let outer = CSSParser.parseList $ fromRight [] $ snd out
    let outest = HTMLBuilder.parseWebpage result outer

    -- print $ outest (173, 76)
    -- putStr $ H.drawCSSTree outer
    Mortar.appIt outest Mortar.initialState

main :: IO ()
main = searchMain
