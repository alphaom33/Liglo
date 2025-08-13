{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Mortar where

import System.IO

import qualified Data.Map as Map
import Data.List.Split
import qualified Data.List as List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class

import Debug.Trace

import System.Console.Terminal.Size

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

tracer str = trace (show str) str

data State = State {
    _line :: Int
    , _strs :: [String]
    , _window :: Window Int
    , _shouldQuit :: Bool
    , _inputList :: String
} deriving (Show, Eq)
$(makeLenses ''State)

initialState = State {
    _line = 0
    , _strs = []
    , _window = Window {width=0, height=0}
    , _shouldQuit = False
    , _inputList = ""
}

getEscapeSequence str = '\x1b' : '[' : str
putEscapeSequence str = putStr $ '\x1b' : '[' : str

hideCursor = putEscapeSequence "?25l"
showCursor = putEscapeSequence "?25h"

resetCursor = putEscapeSequence "H"

clearScreen = do
    putEscapeSequence "2J"
    resetCursor

setForegroundColor r g b = getEscapeSequence $ "38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
setBackgroundColor r g b = getEscapeSequence $ "48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

surroundForegroundColor r g b s = setForegroundColor r g b ++ s ++ resetForeground
surroundBackgroundColor r g b s = setBackgroundColor r g b ++ s ++ resetBackground

surroundBold s = bold ++ s ++ getResetAttrs
surroundItalic s = italic ++ s ++ getResetAttrs

resetForeground = getEscapeSequence "39m"
resetBackground = getEscapeSequence "49m"

bold = getEscapeSequence "1m"
italic = getEscapeSequence "3m"
underline = getEscapeSequence "4m"
strikethrough = getEscapeSequence "9m"

resetAttrs = putEscapeSequence "0m"
getResetAttrs = getEscapeSequence "0m"

removeEscapes str =
    let 
        index = List.elemIndex '\x1b' str
        (before, rest) = splitAt (fromJust index) str
        (_, (_:after)) = splitAt (fromJust $ List.elemIndex 'm' rest) rest
    in case index of
        Nothing -> str
        (Just _) -> removeEscapes $ before ++ after

escapeLength = length . removeEscapes

minit lines num addendum length = _minit lines (length lines) 
    where 
        _minit lines lineLength = if lineLength < num
            then _minit (lines ++ addendum) (lineLength + 1)
            else lines

getKey = reverse <$> _getKey ""
    where
        _getKey chars = do
            char <- getChar
            more <- hReady stdin
            (if more then _getKey else return) $ char : chars

clamp min max num = if num > max
    then max
    else if num < min
        then min
        else num

clampScreen state = set line (clamp 0 ((length $ _strs state) - (height $ _window state) - 1) (_line state)) state

inputMap = Map.fromList [
        ("q", set shouldQuit True)
        , ("j", clampScreen . over line (+1))
        , ("k", clampScreen . over line (pred))
        , ("gg", set line 0)
        , ("G", clampScreen . set line maxBound)
    ]

handleInput state = case inputMap Map.!? _inputList state' of
    Nothing -> state'
    (Just e) -> set inputList "" $ e state'
    where
        checkInputList a = if last a == '\x1b' || not (any ((== a) . take (length a)) (Map.keys inputMap))
            then ""
            else a
        state' = over inputList checkInputList state

wrapWords :: Int -> String -> [String] -> [String]
wrapWords width str out = if escapeLength str > width
    then
        let (tail, rest) = splitAt width str
            (addendum, str1) = splitLastSpace "" $ reverse tail
        in str1 : wrapWords width (addendum ++ rest) out
    else str : out

splitLastSpace stored [] = (stored, "")
splitLastSpace stored (c:cs) = if c == ' '
    then (stored, reverse cs)
    else splitLastSpace (c:stored) cs

appIt str state = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor
    clearScreen

    windowSize <- size :: IO (Maybe (Window Int))
    let w = fromJust windowSize

    let lined = splitOn "\n" str
    let wrapped = foldr (wrapWords $ width w) [] lined

    let state' = set window w $ set strs wrapped state

    drawApp state'
    runApp state'

    resetAttrs
    showCursor
    clearScreen

slice begin end = take (end - begin) . drop begin

drawApp state = do
    let w = _window state

    let minned = map (\ a -> minit a (width w) " " escapeLength) $ minit (_strs state) (height w) [""] length
    let cut = take (height w) . drop (_line state) $ minned

    resetCursor
    putStr $ foldr (++) "" cut
    hFlush stdout

runApp :: State -> IO ()
runApp state = do
    windowSize <- size :: IO (Maybe (Window Int))
    let w = fromJust windowSize

    input <- getKey
    let state' = handleInput $ over inputList (++input) $ set window w state

    drawApp state'

    when (not $ _shouldQuit state') $ runApp state'

