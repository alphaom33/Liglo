{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module HTMLParser where

import Control.Applicative
import Control.Monad (join)
import Control.Monad.Cont (MonadIO(liftIO))
import Debug.Trace (trace)
import Control.Monad.Fix (fix)
import Data.Either (fromRight, isRight)

import Lens.Micro.Mtl ((.=), (%=), use, view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, (^.))

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \case
  []                 -> ([], Left "end of stream")
  (c:cs) | f c       -> (cs, Right c)
         | otherwise -> (cs, Left "did not satisfy")

try :: Parser a -> Parser a
try (P f) = P $ \stream0 -> case f stream0 of
  (_      , Left err) -> (stream0, Left err)
  (stream1, Right a ) -> (stream1, Right a )

consumeUntil :: Parser a -> Parser a
consumeUntil (P f) = P $ fix $ \me stream -> case f stream of
    (_, Left err) -> me (drop 1 stream)
    (rest, Right a) -> (rest, Right a)

orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left err) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)

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
    (_      , Left err) -> (stream, Right [])  -- throws away the error
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

data Tag = Tag {
    _name :: String,
    _children :: [Tag],
    _text :: String
} deriving (Show)
$(makeLenses ''Tag)

char :: Char -> Parser Char
char c = satisfy (== c)

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'
    where dropFirstAndLast _ a _ = a

alpha :: Parser Char
alpha = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz")

numeric :: Parser Char
numeric = satisfy (`elem` "0123456789")

alphanumeric :: Parser Char
alphanumeric = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz0123456789")

consume :: Parser Char
consume = satisfy $ const True

identifier :: Parser String
identifier = (:) <$> alpha <*> manyParser alphanumeric

matchString :: String -> Parser String
matchString = foldr (\ c -> (<*>) ((:) <$> char c)) (pure [])

skipWhitespace :: Parser String
skipWhitespace = manyParser $ char ' '

string :: Parser String
string = (++) <$> ((:) <$> char '"' <*> manyParser (satisfy (/= '"'))) <*> matchString "\""

ack :: [Parser String] -> Parser String
ack [p] = p
ack (p:ps) = (++) <$> p <*> ack ps

skipSetters = manyParser $ ack [skipWhitespace, identifier, skipWhitespace, matchString "=", skipWhitespace, string]

startTag :: Parser String
startTag = dropFirstAndLasts <$> char '<' <*> identifier <*> skipSetters <*> char '>'
    where dropFirstAndLasts _ a _ _ = a

endTag :: String -> Parser String
endTag identifier = dropFirstsAndLast <$> char '<' <*> char '/' <*> matchString identifier <*> char '>'
    where dropFirstsAndLast _ _ a _ = a


comment :: Parser String
comment = (++) <$> matchString "<!--" <*> consumeUntil (matchString "-->")

-- finishTag :: String -> (String, String, [Tag])
-- finishTag toFinish = do
--     let (rest, text) = parse (manyParser $ satisfy (/= '<')) $ parseComment toFinish
--     let text' = fromRight "" text
--     let (rest', name) = parse (try startTag) $ parseComment rest
--     case name of
--         Right e -> do
--             let (rest'', child) = parseTag e $ parseComment rest'
--             let (rest''', texts, childs) = finishTag $ parseComment rest''
--             (rest''', text' ++ texts, child:childs)
--         Left _ -> (rest', text', [])

checkText :: String -> Tag -> (String, Tag, Bool)
checkText toCheck current =
    let
        (rest, result) = parse (someParser $ satisfy (/= '<')) toCheck
    in
        case result of
            Right a -> (rest,  set text (view text current ++ a) current, True)
            Left _ -> (toCheck, current, False)

checkTag :: String -> Tag -> (String, Tag, Bool)
checkTag toCheck current =
    let
        (rest, result) = parse startTag toCheck
    in
        case result of
            Right a -> let (rest', tag) = parseTag toCheck in (rest', set children (tag : view children current) current, True)
            Left _ -> (toCheck, current, False)

checkComment :: String -> Tag -> (String, Tag, Bool)
checkComment toCheck current = 
    let 
        (rest, result) = parse (try comment) toCheck
    in
        (rest, current, isRight result)

parseBody :: String -> Tag -> (String, Tag)
parseBody rest tag = do
    let (rest', tag', textSucceeded) = checkText rest tag
    let (rest'', tag'', commentSucceeded) = checkTag rest' tag'
    let (rest''', tag''', startSucceeded) = checkComment rest'' tag''
    if textSucceeded || commentSucceeded || startSucceeded then
        parseBody (trace rest''' rest''') tag'''
    else
        (rest, tag)

parseTag :: String -> (String, Tag)
parseTag toParse = do
    let (rest, _name) = parse startTag toParse
    let name = fromRight "" _name
    let (rest', tag) = parseBody rest Tag {_name=name, _children=[], _text=""}
    let (rest'', _) = parse (endTag name) rest'
    (rest'', tag)

parseConsume toConsume string = fst $ parse (matchString toConsume) string

parseString :: String -> (String, Tag)
parseString string = do
    parseTag $ parseConsume "<!DOCTYPE html>" string
