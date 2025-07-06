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
    , _out :: [Widget Name]
    , _currentHBox :: [Widget Name]
    , _currentText :: String
}
$(makeLenses ''BuilderData)
builderData emittedTokens = BuilderData {_openTags=[], _out=[], _toRead=emittedTokens, _currentHBox=[], _currentText=""}

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
buildHtml state = [viewport Viewport1 Vertical $ vBox $ _out $ _buildHtml $ builderData $ _emittedTokens state]

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:emittedTokens) -> _buildHtml $ (if
        | _selfClosing t -> if _tagName t == "br"
            then appendHbox mhm'
            else mhm'
        | _opening t -> over openTags (_tagName t:) $ if not (null $ _currentText mhm)
            then (endings t mhm')
            else mhm'
        | not $ _opening t -> over openTags (drop 1) $ endings t mhm'
        | otherwise -> mhm')
        where
            appendHbox mhm = over out (++[hBox $ _currentHBox mhm]) $ set currentHBox [] mhm
            endings t mhm = if _tagName t `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div"]
                then appendHbox mhm'
                else over currentHBox (++[withAttr (attrName $ _tagName t) $ str $ reverse $ _currentText mhm]) mhm' 
                where mhm' = set currentText "" mhm
    (Character c:emittedTokens) -> _buildHtml $ (if c `elem` " \n\t"
        then killWhitespace
        else if length (_openTags mhm) > 0 && null (["head", "meta", "link", "script", "style", "select"] `intersect` _openTags mhm)
            then \ mhm -> over currentText (c:) mhm
            else id) mhm'
    (e:emittedTokens) -> _buildHtml mhm'
    [] -> mhm
    where (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)

killWhitespace mhm = _killWhitespace $ (if null (_out mhm) || (null (_currentText mhm) && null (_currentHBox mhm))
    then id
    else over currentText (' ':)) mhm

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
    , appAttrMap = \ _ -> attrMap defAttr [
        (attrName "b", defAttr `withStyle` V.bold `withBackColor` black)
        ]
    }

