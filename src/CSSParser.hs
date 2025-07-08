module CSSParser where

import Text.Regex.TDFA
import Control.Applicative
import Data.Function

import HTMLParser (tracer)

type Error = String
data Parser a = Parser { parse :: String -> (String, Either Error a)}

instance Functor Parser where
    fmap f (Parser f2) = Parser $ \ stream -> case f2 stream of
        (rest, Right out) -> (rest, Right $ f out)
        (rest, Left err) -> (rest, Left err)

instance Applicative Parser where
    pure a = Parser $ \ stream -> (stream, Right a)
    Parser ff <*> Parser xx = Parser $ \ stream -> case ff stream of
        (rest, Left err) -> (rest, Left err)
        (rest, Right f) -> case xx rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right x) -> (rest', Right $ f x)

instance Alternative Parser where
    empty = Parser $ \ stream -> (stream, Left "empty")
    (<|>) = orElse
    many = manyParser

orElse (Parser f0) (Parser f1) = Parser $ \ stream -> case f0 stream of
    (rest, Right out) -> (rest, Right out)
    (rest, Left _) -> f1 stream

manyParser (Parser f) = Parser $ fix $ \ this stream -> case f stream of
    (_, Left _) -> (stream, Right [])
    (rest, Right a) -> case this rest of
        (rest', Left err) -> (rest', Left err)
        (rest', Right as) -> (rest', Right $ a : as)

satisfy f = Parser $ \ stream -> case stream of
    [] -> ("", Left "end of stream")
    (c:cs) 
        | f c -> (cs, Right c)
        | otherwise -> (cs, Left "did not satisfy")

replaceAll :: String -> String -> String -> String
replaceAll regex toInsert str =
    let
        (before, checked, after) = str =~ regex
    in if checked /= ""
        then before ++ toInsert ++ replaceAll regex toInsert after
        else before

killRegisteredNurse :: String -> String
killRegisteredNurse = replaceAll "\r\n" "\n"

killComments :: String -> String
killComments = replaceAll "/\\*.*\\*/" ""

preProcess str = map (\ c -> case c of -- also replace surrogates somehow
    '\r' -> '\n'
    '\f' -> '\n'
    '\0' -> '\xfffd'
    _ -> c) $ killComments $ killRegisteredNurse str

matchChar c = satisfy (== c)

matchString (c:cs) = (:) <$> matchChar c <*> matchString cs

parseString str =
    let preProcessed = preProcess str
    in parse (manyParser ((matchChar 'a') <|> (matchChar 'b'))) preProcessed

