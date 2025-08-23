{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

getAttr t name = filter (\ (Attribute (n, _)) -> n == name) (_attrs t) L.!? 0

grabStylesheets :: [Token] -> [Token]
grabStylesheets = filter (\case
    (TagToken t) -> checkAttr t "rel" "stylesheet" && checkAttr t "type" "text/css"
    _ -> False)
    where
        checkAttr t attrName str = case getAttr t attrName of
            Nothing -> False
            (Just (Attribute (_, v))) -> v == str

linkStyleTags = map $ \case
    (TagToken t) -> case getAttr t "href" of
        (Just (Attribute (_, v))) -> v

nomUrl :: String -> IO String
nomUrl url = do
    a <- parseRequest url
    b <- httpLBS a
    pure $ unpack $ getResponseBody b

takeStyles :: String -> [Token] -> String
takeStyles out [] = out
takeStyles out (next:rest) = case next of
    (TagToken t) -> if _opening t && _tagName t == "style"
        then let (out', rest') = takeChars "" rest in takeStyles (out ++ out') rest'
        else takeStyles out rest
    _ -> takeStyles out rest

takeChars out (next:rest) = case next of
    (Character c) -> takeChars (c : out) rest
    (TagToken t) -> if not (_opening t) && _tagName t == "style"
        then (reverse out, rest)
        else takeChars out rest

main = do
    -- args <- getArgs
    -- let arged = map (\ c -> if c == ' ' then '+' else c) $ concat args
    -- key <- lookupEnv "GOOGLE_API_KEY"
    -- let apiKey = fromJust key
    -- finalState <- defaultMain S.app $ S.initialState apiKey $ head args
    --
    -- when (S._curQuery finalState == head args) (do
    --     print "exited forcefully"
    --     exitSuccess)

    let finalURL = "https://hoogle.haskell.org" --S._curQuery finalState

    -- a <- parseRequest finalURL
    -- b <- httpLBS a
    -- let asdf = unpack (getResponseBody b)
    asdf <- readFile "asdf.html"
    -- writeFile "asdf.html" asdf
    let result = _emitted $ parseString asdf

    -- let styleLinks = linkStyleTags $ grabStylesheets $ _emitted result
    -- yankedStyles <- foldr (\ a b -> (++) <$> nomUrl (finalURL ++ '/':a) <*> b) (pure []) styleLinks
    --
    -- let styleInlines = takeStyles "" $ _emitted result
    --
    -- let str = yankedStyles ++ styleInlines
    -- writeFile "asdf.css" str
    str <- readFile "asdf.css"

    let out = CT.parseString str
    let outer = CP.parseList $ fromRight [] $ snd out
    let outest = H.parseWebpage result outer

    Mortar.appIt outest Mortar.initialState
