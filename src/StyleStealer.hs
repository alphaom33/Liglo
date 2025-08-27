{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StyleStealer where

import qualified Data.List as L

import HTMLParser
import Network.HTTP.Client
import Network.HTTP.Simple
import Data.ByteString.Lazy.Char8 (unpack)

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

nomUrl :: String -> String -> IO String
nomUrl startUrl url = do
    a <- parseRequest $ tracer $ resolveUrl startUrl url
    b <- httpLBS a
    pure $ unpack $ getResponseBody b

resolveUrl startUrl url = case url of
    ('.':'.':'/':rest) -> _resolveUrl (killSlash $ killSlash startUrl) rest
    _ -> startUrl ++ '/':url

_resolveUrl startUrl url = case url of
    ('.':'.':'/':rest) -> _resolveUrl (killSlash startUrl) rest
    _ -> startUrl ++ '/':url

killSlash = reverse . _killSlash . reverse
_killSlash str = case str of
    [] -> str
    ('/':rest) -> rest
    (_:rest) -> _killSlash rest

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

