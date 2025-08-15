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

main = do
    args <- getArgs
    let arged = map (\ c -> if c == ' ' then '+' else c) $ concat args
    key <- lookupEnv "GOOGLE_API_KEY"
    let apiKey = fromJust key
    finalState <- defaultMain S.app $ S.initialState apiKey $ head args

    when (S._curQuery finalState == head args) (do
        print "exited forcefully"
        exitSuccess)

    a <- parseRequest $ S._curQuery finalState
    b <- httpLBS a
    let asdf = unpack (getResponseBody b)
    let result = parseString asdf

    let styleLinks = linkStyleTags $ grabStylesheets $ _emitted result
    str <- foldr (\ a b -> (++) <$> nomUrl ("https://hoogle.haskell.org" ++ '/':a) <*> b) (pure []) styleLinks
    let out = CT.parseString str
    let outer = CP.parseList $ fromRight [] $ snd out
    let outest = H.parseWebpage (_emitted result) outer

    Mortar.appIt outest Mortar.initialState

