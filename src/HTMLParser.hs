{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module HTMLParser where

import Control.Applicative (Alternative, empty, many, some, (<|>))
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

matchThrough :: Char -> Parser String
matchThrough c = dropLast <$> manyParser (notChar c) <*> char c
    where dropLast a _ = a

discardDels :: a -> b -> c -> b
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

matchTagIgnoreCase :: String -> Parser String
matchTagIgnoreCase str = discardDels <$> char '<' <*> matchStringIgnoreCase str <*> char '>'

checkDels :: String -> [Char] -> (String, Either Error String)
checkDels stream [] = (stream, Left "Expected ")
checkDels stream (del:dels) =
    let
        (P op) = char del
    in case op stream of
        (_, Left err) -> checkDels stream dels
        (stream', Right _) -> (stream', Right (del:[]))

string :: Parser String
string = P $ \stream -> 
    let
        dels = "'\""
    in case checkDels stream dels of
        (stream', Right del) -> let (P end) = matchThrough (del!!0) in end stream'
        (stream', Left err) -> (stream', Left $ err ++ dels)

data Attribute = Attribute (String, String) deriving Show

attribute :: Parser Attribute
attribute = P $ \cs -> (cs, Right $ Attribute ("", ""))

data TagType = Select | Template | Table | TD | TH | TR | TBody | THead | TFoot | Caption | Colgroup | Head | Body | FrameSet | Html | External | TestType deriving Show

data Tag = Tag {
    tagType :: TagType
    , attrs :: [Attribute]
} deriving Show

strToType :: String -> TagType
strToType str = case str of 
    "select" -> Select
    _ -> External

makeTag :: String -> [Attribute] -> Tag
makeTag _name _attrs = Tag {tagType=strToType _name, attrs=_attrs}

tag :: Parser Tag
tag = discardDels <$> char '<' <*> (makeTag <$> string <*> manyParser attribute) <*> char '>'

doctype :: Parser String
doctype = matchTagIgnoreCase "!doctype html"

preProcess :: String -> String
preProcess str = filter (/= '\r') str

data InsertionMode = Initial | InSelect | InSelectInTable | InCell | InRow | InTableBody | InCaption | InColgroup | InTable | InHead | InBody | InFrameSet | BeforeHead | AfterHead | TestMode deriving (Show, Eq)
data State = State {
    _openElements :: [Tag]
    , _mode :: InsertionMode
    , _templateModes :: [InsertionMode]
    , _headPointer :: Maybe Tag
    , _formPointer :: Maybe Tag
} deriving (Show)
$(makeLenses ''State)

doSelect :: [Tag] -> InsertionMode
doSelect t = _doSelect $ reverse t

_doSelect :: [Tag] -> InsertionMode
_doSelect [] = Initial
_doSelect [_] = InSelect
_doSelect (el:els) =
    case tracer $ tagType el of
        Template -> InSelect
        Table -> InSelectInTable
        _ -> _doSelect els

resetInsertionMode :: State -> State
resetInsertionMode state = _resetInsertionMode ((length $ view openElements state) - 1) state


_resetInsertionMode :: Int -> State -> State
_resetInsertionMode idx state =
    let
        opened = _openElements state
        isLast = idx == 0
    in case (isLast, tagType (opened!!idx)) of
        (_, Select) -> set mode (doSelect opened) state
        (False, TD) -> set mode InCell state
        (False, TH) -> set mode InCell state
        (_, TR) -> set mode InRow state
        (_, TBody) -> set mode InTableBody state
        (_, THead) -> set mode InTableBody state
        (_, TFoot) -> set mode InTableBody state
        (_, Caption) -> set mode InCaption state
        (_, Colgroup) -> set mode InColgroup state
        (_, Table) -> set mode InTable state
        (_, Template) -> set mode (head $ view templateModes state) state
        (_, Head) -> set mode InHead state
        (_, Body) -> set mode InBody state
        (_, FrameSet) -> set mode InFrameSet state
        (_, Html) -> set mode (case view headPointer state of
                Nothing -> BeforeHead
                (Just _) -> AfterHead)
                state
        (True, _) -> set mode InBody state
        _ -> _resetInsertionMode (idx - 1) state

parseString :: String -> (String, Either Error String)
parseString str = ("", Right "")
