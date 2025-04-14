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


data Child = ChildTag Tag | ChildText String

instance Show Child where
    show (ChildTag a) = show a
    show (ChildText a) = a ++ "\n"

data Tag = Tag {
    _name :: String,
    _children :: [Child]
}
instance Show Tag where
    show (Tag name children) = "<" ++ name ++ ">\n" ++ concatMap show (reverse children) ++ "</" ++ name ++ ">\n"
$(makeLenses ''Tag)


char :: Char -> Parser Char
char c = satisfy (== c)

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'
    where dropFirstAndLast _ a _ = a

alpha :: Parser Char
alpha = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz-")

numeric :: Parser Char
numeric = satisfy (`elem` "0123456789")

alphanumeric :: Parser Char
alphanumeric = alpha <||> numeric

consume :: Parser Char
consume = satisfy $ const True

identifier :: Parser String
identifier = someParser alphanumeric

matchString :: String -> Parser String
matchString = foldr (\ c -> (<*>) ((:) <$> char c)) (pure [])

whiteSpace :: Parser Char
whiteSpace = char ' ' <||> char '\n' <||> char '\t'

skipWhitespace :: Parser String
skipWhitespace = manyParser whiteSpace

string :: Parser String
string = (++) <$> ((:) <$> char '"' <*> manyParser (satisfy (/= '"'))) <*> matchString "\""

ack :: [Parser String] -> Parser String
ack [] = pure []
ack [p] = p
ack (p:ps) = (++) <$> p <*> ack ps

skipSetters :: Parser [String]
skipSetters = manyParser $ ack [skipWhitespace, identifier, skipWhitespace, matchString "=", skipWhitespace, string]

startTag :: Parser String
startTag = dropFirstAndLasts <$> char '<' <*> identifier <*> skipSetters <*> skipWhitespace <*> char '>'
    where dropFirstAndLasts _ a _ _ _ = a

middleTag :: Parser String
middleTag = dropFirstsAndLastss <$> char '<' <*> identifier <*> skipSetters <*> skipWhitespace <*> char '/' <*> char '>'
    where dropFirstsAndLastss _ a _ _ _ _ = a

endTag :: String -> Parser String
endTag tagName = dropFirstsAndLast <$> char '<' <*> char '/' <*> matchString tagName <*> skipWhitespace <*> char '>'
    where dropFirstsAndLast _ _ a _ _ = a


comment :: Parser String
comment = (++) <$> matchString "<!--" <*> consumeUntil (matchString "-->")

checkText :: Tag -> Parser Tag
checkText current = P $ \ cs ->
    let
        (P doUntilLT) = someParser $ satisfy (/= '<')
    in
        case doUntilLT cs of
            (rest, Right a) -> (rest,  Right $ set children (ChildText a : view children current) current)
            (rest, Left _) -> (rest, Left "noCheckText")

checkTag :: Tag -> Parser Tag
checkTag current = P $ \ cs ->
    let
        (P doStartTag) = startTag
        (P doParseTag) = parseTag
    in case doStartTag cs of
        (_, Right _) -> case doParseTag cs of
            (rest, Right c) -> (rest, Right $ set children (ChildTag c : view children current) current)
            (rest, Left e) -> (rest, Left e)
        (rest, Left e) -> (rest, Left $ "noCheckTag " ++ e)

eatTagged :: Parser a -> Tag -> Parser Tag
eatTagged toEat current = P $ \ cs ->
    let (P doToEat) = toEat
    in case doToEat cs of
        (rest, Right _) -> (rest, Right current)
        (rest, Left e) -> (rest, Left $ "eatTagged failed " ++ e)

switchGive :: [String -> Tag -> (String, Tag, Bool)] -> String -> Tag -> [Bool] -> (String, Tag, Bool)
switchGive [] rest tag successes = (rest, tag, or successes)
switchGive (toDo:toDos) rest tag successes =
    let (rest', tag', succeeded) = toDo rest tag
    in switchGive toDos rest' tag' (succeeded:successes)


tagBody :: Tag -> Parser Tag
tagBody tag = P $ \ cs -> do
    let (P doChecks) = try $ checkText tag <||> checkTag tag <||> eatTagged comment tag <||> eatTagged middleTag tag
    case doChecks cs of
        (rest, Right a) ->
            let (P doParseBody) = tagBody a
            in doParseBody rest
        (rest, Left _) -> (rest, Right tag)

parseTag :: Parser Tag
parseTag = P $ \ cs -> do
    let (P doStartTag) = startTag
    case doStartTag cs of
        (rest, Right tagName) ->
            let
                (P doTagBody) = tagBody Tag {_name=tagName, _children=[]}
                (P doEndTag) = try $ endTag tagName
                (rest', result) = doTagBody rest
            in
                case doEndTag rest' of
                    (rest'', Right _) -> (rest'', result)
                    (rest'', Left _) -> (rest'', result)
        (rest, Left a) -> (rest, Left a)

killDoctype :: Parser [Char]
killDoctype = (++) <$>  matchString "<!DOCTYPE html" <*> consumeUntil (matchString ">")

parseString :: String -> (String, Either Error Tag)
parseString = parse (drops <$> killDoctype <*> manyParser (middleTag <||> ((:) <$> whiteSpace <*> pure []) <||> comment) <*> parseTag)
    where drops _ _ a = a
