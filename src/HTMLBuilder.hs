{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module HTMLBuilder where

import Data.List
import Data.Char

import Brick
import Graphics.Vty as V

import Text.Wrap

import Brick.AttrMap (attrMap)
import Brick.Widgets.Center (center, vCenter, hCenter)
import Brick.Widgets.Border (border)

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

import CharacterReferences
import HTMLParser (Token(..), _selfClosing, _opening, _tagName, tracer)
import SearchApp (Name(..))
import Message

data BuilderData = BuilderData {
    _toRead :: [Token]
    , _openTags :: [String]
    , _out :: String
} deriving (Show)
$(makeLenses ''BuilderData)
builderData emittedTokens = BuilderData {_openTags=[], _out=[], _toRead=emittedTokens}

data State = State {
    _emittedTokens :: [Token]
} deriving (Show)

buildHtml :: State -> [Widget Name]
buildHtml state = [viewport Viewport1 Vertical $ strWrap $ reverse $ _out $ _buildHtml $ builderData (_emittedTokens state)]

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:emittedTokens) -> _buildHtml $ (if
        | _selfClosing t -> if _tagName t == "br"
            then over out ('\n':) mhm'
            else mhm'
        | _opening t -> over openTags (_tagName t:) mhm'
        | not $ _opening t -> (if _tagName t `elem` ["li", "h1", "p"]
            then over out ('\n':)
            else id) $ over openTags (drop 1) mhm'
        | otherwise -> mhm')
    (Character c:emittedTokens) -> _buildHtml $ if c `elem` " \n\t"
        then killWhitespace mhm'
        else (if length (_openTags mhm) > 0 && (_openTags mhm !! 0) `elem` ["p", "a", "div", "li", "h1"]
            then over out (c:)
            else id) mhm'
    (e:emittedTokens) -> _buildHtml mhm'
    [] -> mhm
    where (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)

killWhitespace mhm = _killWhitespace $ (if length (_out mhm) == 0 || _out mhm !! 0 `elem` " \n\t"
    then id
    else over out (' ':)) mhm

_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)


initialState :: [Token] -> State
initialState p_emittedTokens = State {
    _emittedTokens = p_emittedTokens
}

start :: EventM Name State ()
start = return ()

handler (VtyEvent (EvKey k _)) =
    let vp = viewportScroll Viewport1
    in case k of
        KChar 'k' -> vScrollBy vp (-1)
        KChar 'j' -> vScrollBy vp 1
        KChar 'q' -> halt
        _ -> return ()

handler _ = return ()

app :: App State Message Name
app = App {
    appStartEvent = start
    , appChooseCursor = neverShowCursor
    , appDraw = buildHtml
    , appHandleEvent = handler
    , appAttrMap = \ _ -> attrMap defAttr []
    }

