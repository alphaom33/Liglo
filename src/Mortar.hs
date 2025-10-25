{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Mortar where

import System.IO

import qualified Data.Map as Map
import Data.List.Split
import qualified Data.List as List
import Data.Maybe
import Control.Monad

import Debug.Trace

import System.Console.Terminal.Size

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

tracer :: Show a => a -> a
tracer str = trace (show str) str

data State = State {
    _line :: Int
    , _strs :: [String]
    , _window :: Window Int
    , _shouldQuit :: Bool
    , _inputList :: String
} deriving (Show, Eq)
$(makeLenses ''State)

initialState :: State
initialState = State {
    _line = 0
    , _strs = []
    , _window = Window {width=0, height=0}
    , _shouldQuit = False
    , _inputList = ""
}

getEscapeSequence :: [Char] -> [Char]
getEscapeSequence str = '\ESC' : '[' : str
putEscapeSequence :: [Char] -> IO ()
putEscapeSequence str = putStr $ '\ESC' : '[' : str

hideCursor :: IO ()
hideCursor = putEscapeSequence "?25l"
showCursor :: IO ()
showCursor = putEscapeSequence "?25h"

resetCursor :: IO ()
resetCursor = putEscapeSequence "H"

clearScreen :: IO ()
clearScreen = do
    putEscapeSequence "2J"
    resetCursor

setForegroundColor :: (Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> [Char]
setForegroundColor r g b = getEscapeSequence $ "38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
setBackgroundColor :: (Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> [Char]
setBackgroundColor r g b = getEscapeSequence $ "48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

surroundForegroundColor :: (Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> [Char] -> [Char]
surroundForegroundColor r g b s = setForegroundColor r g b ++ s ++ resetForeground
surroundBackgroundColor :: (Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> [Char] -> [Char]
surroundBackgroundColor r g b s = setBackgroundColor r g b ++ s ++ resetBackground

surroundBold :: [Char] -> [Char]
surroundBold s = bold ++ s ++ resetBold
surroundItalic :: [Char] -> [Char]
surroundItalic s = italic ++ s ++ resetItalic
surroundUnderline :: [Char] -> [Char]
surroundUnderline s = underline ++ s ++ resetUnderline
surroundStrikethrough :: [Char] -> [Char]
surroundStrikethrough s = strikethrough ++ s ++ resetStrikethrough

resetForeground :: [Char]
resetForeground = getEscapeSequence "39m"
resetBackground :: [Char]
resetBackground = getEscapeSequence "49m"

bold :: [Char]
bold = getEscapeSequence "1m"
italic :: [Char]
italic = getEscapeSequence "3m"
underline :: [Char]
underline = getEscapeSequence "4m"
strikethrough :: [Char]
strikethrough = getEscapeSequence "9m"

resetBold :: [Char]
resetBold = getEscapeSequence "22m"
resetItalic :: [Char]
resetItalic = getEscapeSequence "23m"
resetUnderline :: [Char]
resetUnderline = getEscapeSequence "24m"
resetStrikethrough :: [Char]
resetStrikethrough = getEscapeSequence "29m"

resetAttrs :: IO ()
resetAttrs = putEscapeSequence "0m"
getResetAttrs :: [Char]
getResetAttrs = getEscapeSequence "0m"

removeEscapes :: [Char] -> [Char]
removeEscapes str =
    let 
        index = List.elemIndex '\ESC' str
        (before, rest) = splitAt (fromJust index) str
        (_, _:after) = splitAt (fromJust $ List.elemIndex 'm' rest) rest
    in case index of
        Nothing -> str
        (Just _) -> removeEscapes $ before ++ after

escapeLength :: [Char] -> Int
escapeLength = length . removeEscapes

minit :: (Ord p, Num p) =>  p -> [a] -> ([a] -> p) -> [a] -> [a]
minit num addendum length lines = _minit lines (length lines) 
    where 
        _minit lines lineLength = if lineLength < num
            then _minit (lines ++ addendum) (lineLength + 1)
            else lines

getKey :: IO [Char]
getKey = reverse <$> _getKey ""
    where
        _getKey chars = do
            char <- getChar
            more <- hReady stdin
            (if more then _getKey else return) $ char : chars

clamp :: Ord a => a -> a -> a -> a
clamp min max num 
    | num > max = max
    | num < min = min
    | otherwise = num

clampScreen :: State -> State
clampScreen state = set line (clamp 0 (length (_strs state) - height (_window state) - 1) (_line state)) state

inputMap :: Map.Map [Char] (State -> State)
inputMap = Map.fromList [
        ("q", set shouldQuit True)
        , ("j", clampScreen . over line (+1))
        , ("k", clampScreen . over line pred)
        , ("gg", set line 0)
        , ("G", clampScreen . set line maxBound)
    ]

handleInput :: State -> State
handleInput state = case inputMap Map.!? _inputList state' of
    Nothing -> state'
    (Just e) -> set inputList "" $ e state'
    where
        checkInputList a = if last a == '\x1b' || not (any ((== a) . take (length a)) (Map.keys inputMap))
            then ""
            else a
        state' = over inputList checkInputList state

wrapWords :: Int -> String -> [String]

wrapWords width str = go 0 str "" "" 
    where 
        go count str word out = if count > width
            then drop 1 out : go (escapeLength $ reverse word) str word ""
            else case str of
                [] -> [drop 1 addWord]
                (' ':str') -> go (count + 1) str' "" addWord
                ('\ESC':_) ->
                    let escape = eatEscape str
                    in go count (drop (length escape) str) (reverse escape ++ word) out
                (s:str') -> go (count + 1) str' (s:word) out
            where addWord = out ++ " " ++ reverse word

        eatEscape ('m':_) = "m"
        eatEscape (c:str') = c : eatEscape str'


splitLastSpace :: [Char] -> [Char] -> ([Char], [Char])
splitLastSpace stored [] = (stored, "")
splitLastSpace stored (c:cs) = if c == ' '
    then (stored, reverse cs)
    else splitLastSpace (c:stored) cs

appIt :: [Char] -> State -> IO ()
appIt str state = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor
    clearScreen

    windowSize <- size :: IO (Maybe (Window Int))

    let 
        w = fromJust windowSize
        lined = splitOn "\n" str
        wrapped = foldr (\ a b -> wrapWords (width w) a ++ b) [] lined
        minned = map (minit (width w) " " escapeLength) wrapped
        state' = set window w $ set strs minned state

    drawApp state'
    runApp state'

    resetAttrs
    showCursor
    -- clearScreen

slice :: Int -> Int -> [a] -> [a]
slice begin end = take (end - begin) . drop begin

drawApp :: State -> IO ()
drawApp state = do
    let w = _window state

    let minned = minit (height w) [""] length (_strs state)
    let cut = take (height w) . drop (_line state) $ minned

    resetCursor
    putStr $ concat cut
    hFlush stdout

runApp :: State -> IO ()
runApp state = do
    windowSize <- size :: IO (Maybe (Window Int))
    let w = fromJust windowSize

    input <- getKey
    let state' = handleInput $ over inputList (++input) $ set window w state

    drawApp state'

    unless (_shouldQuit state') $ runApp state'

