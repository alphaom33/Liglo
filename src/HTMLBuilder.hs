{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module HTMLBuilder where

import Data.List
import Data.Char
import Data.Map (fromList, (!), keys, Map(..))
import Control.Monad (when)

import Brick
import Graphics.Vty as V

import Text.Wrap

import Brick.AttrMap (attrMap)
import Brick.Widgets.Center (center, vCenter, hCenter)
import Brick.Widgets.Border (border)

import Lens.Micro.Mtl ((.=), (%=), use, view)
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
    , _hotkey :: String
} deriving (Show)
$(makeLenses ''State)

hotkeys :: Map String (EventM Name State ())
hotkeys = fromList [
    ("j", let vp = viewportScroll Viewport1 in vScrollBy vp (1))
    , ("k", let vp = viewportScroll Viewport1 in vScrollBy vp (-1))
    , ("u", let vp = viewportScroll Viewport1 in vScrollPage vp Up)
    , ("d", let vp = viewportScroll Viewport1 in vScrollPage vp Down)
    , ("gg", let vp = viewportScroll Viewport1 in vScrollToBeginning vp)
    , ("G", let vp = viewportScroll Viewport1 in vScrollToEnd vp)
    , ("q", halt)
    ]

buildHtml :: State -> [Widget Name]
buildHtml state = [viewport Viewport1 Vertical $ strWrap $ reverse $ _out $ _buildHtml $ builderData (_emittedTokens state)]

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:emittedTokens) -> _buildHtml $ (if
        | _selfClosing t -> if _tagName t == "br"
            then over out ('\n':) mhm'
            else mhm'
        | _opening t -> over openTags (_tagName t:) mhm'
        | not $ _opening t -> (if _tagName t `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div"]
            then over out ('\n':)
            else id) $ over openTags (drop 1) mhm'
        | otherwise -> mhm')
    (Character c:emittedTokens) -> _buildHtml $ if c `elem` " \n\t"
        then killWhitespace mhm'
        else (if length (_openTags mhm) > 0 && length (["head", "meta", "link", "script", "style", "select"] `intersect` _openTags mhm) == 0
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
    , _hotkey = ""
}

start :: EventM Name State ()
start = return ()

handler :: BrickEvent Name Message -> EventM Name State ()

handler (VtyEvent (EvKey k m)) = case k of
    (KChar c) -> do
        hotkey %= (((if MShift `elem` m then toUpper else id) c):)
        _hotkey <- use hotkey
        when (_hotkey `elem` (keys hotkeys)) (do
            hotkey .= ""
            hotkeys ! _hotkey)
    KEsc -> hotkey .= ""
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

