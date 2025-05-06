{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module HTMLParser where

import Control.Applicative
import Debug.Trace (trace)
import Control.Monad.Fix (fix)
import Data.Either (fromRight, isRight, fromLeft)

import Lens.Micro.Mtl (view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set)
import Data.Char (toUpper, toLower)

tracer :: Show a => a -> a
tracer a = trace (show a) a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \case
  []                 -> ([], Left "end of stream")
  (c:cs) | f c       -> (cs, Right c)
         | otherwise -> (cs, Left $ show c ++ "did not satisfy")

try :: Parser a -> Parser a
try (P f) = P $ \stream0 -> case f stream0 of
  (_      , Left err) -> (stream0, Left err)
  (stream1, Right a ) -> (stream1, Right a )

consumeUntil :: Parser a -> Parser a
consumeUntil (P f) = P $ fix $ \me stream -> case f stream of
    ("", Left a) -> ("", Left a)
    (_, Left _) -> me (drop 1 stream)
    (rest, Right a) -> (rest, Right a)

orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left _) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)

tryElse, (<||>) :: Parser a -> Parser a -> Parser a
tryElse f1 f2 = try f1 <|> try f2
(<||>) = tryElse

type Error = String
newtype Parser a = P { parse :: String -> (String, Either Error a) }

instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

instance Applicative Parser where
  pure a = P (, Right a)
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)

instance Alternative Parser where
  empty = P (, Left "empty")
  (<|>) = orElse

  many = manyParser
  some = someParser

-- | 0 or more
manyParser :: Parser a -> Parser [a]
manyParser (P f) = P go where
  go stream = case f stream of
    (_      , Left _) -> (stream, Right [])  -- throws away the error
    (stream', Right a ) -> case go stream' of
      (streamFin, Left err) -> (streamFin, Left err)
      (streamFin, Right as) -> (streamFin, Right (a : as))

-- | 1 or more
someParser :: Parser a -> Parser [a]
someParser (P f) = P $ \stream -> case f stream of
  (stream', Left err) -> (stream', Left err)
  (stream', Right a ) ->
    let (P fmany) = manyParser (P f)
    in case fmany stream' of
      (stream'', Left err) -> (stream'', Left err)
      (stream'', Right as) -> (stream'', Right (a:as))

matchThrough c = dropLast <$> manyParser (notChar c) <*> char c
    where dropLast a _ = a

discardDels _ a _ = a

char :: Char -> Parser Char
char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toLower c) <||> char (toUpper c)

matchString :: String -> Parser String
matchString = foldr (\ c -> (<*>) ((:) <$> char c)) (pure [])

matchStringIgnoreCase :: String -> Parser String
matchStringIgnoreCase = foldr (\ c -> (<*>) ((:) <$> charIgnoreCase c)) (pure [])

matchTagIgnoreCase str = discardDels <$> char '<' <*> matchStringIgnoreCase str <*> char '>'

checkDels :: String -> [Char] -> (String, Either Error String)
checkDels stream (del:dels) =
    let
        (P op) = char del
    in case op stream of
        (_, Left err) -> checkDels stream dels
        (stream', Right _) -> (stream', Right (del:[]))

string = P $ \stream -> 
    let
        dels = "'\""
    in case checkDels stream dels of
        (stream', Right del) -> let (P end) = matchThrough (del!!0) in end stream'
        (stream', Left err) -> (stream', Left $ err ++ dels)

tag = dropDels <$> char '<' <*> ((:) <$> string <*> manyParser attribute) <*> char '>'

doctype = matchTagIgnoreCase "!doctype html"

parseString = parse doctype