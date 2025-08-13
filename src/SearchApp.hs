{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module SearchApp where

import Text.Wrap (WrapSettings(..), FillScope(..), FillStrategy(..))

import Brick
import Graphics.Vty as V

import Lens.Micro.Mtl ((.=), (%=), use, view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set)

import Control.Monad (when)
import Data.Maybe (fromJust)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (border)
import Brick.Forms (editTextField, renderForm, newForm, Form (formState, formFocus), handleFormEvent)
import Data.Text (Text, unpack)
import Brick.Focus (focusRingCursor)
import Message (Message())
import Item
import Network (getResponse)
import Control.Monad.IO.Class (liftIO)

newtype Search = Search {
    _query :: Text
} deriving (Eq, Ord, Show)
$(makeLenses ''Search)

data Name = Viewport1 | QueryField deriving (Show, Eq, Ord)

data State = State {
    _curQuery :: String,
    _key :: String,
    _page :: Int,
    _mytems :: [Item],
    _cursorPos :: Int,
    _cursorScroll :: Int,
    _canMove :: Bool,
    _form :: Form Search Message Name
}
instance Show State where
    show a = "_mytems: " ++ show (_mytems a) ++ " _cursorPos: " ++ show (_cursorPos a) ++ " _canMove: " ++ show (_canMove a) ++ "\n"

$(makeLenses ''State)

wrapSettings :: WrapSettings
wrapSettings = WrapSettings {
    preserveIndentation = True
    , breakLongWords = True
    , fillStrategy = NoFill
    , fillScope = FillAfterFirst
    }

mkForm :: Search -> Form Search e Name
mkForm = newForm [editTextField query QueryField (Just 1)]

drawForm :: State -> Widget Name
drawForm state =
    let attr = withAttr $ attrName "searchBox"
    in attr $ border $ hCenter $ renderForm $ view form state

--TODO variable dst
boxThing :: Int -> Item -> Widget Name
boxThing index item =
    let
        el yep = yep <=> str ("  " ++ snippet item) <=> str " "
        label = str (title item)
    in if index == 0 
        then el (withAttr (attrName "current") label) 
        else el label

makeBoxes :: State -> [Widget Name]
makeBoxes state = zipWith boxThing (map (view cursorPos state-) [0..]) (view mytems state)

drawUI :: State -> [Widget Name]
drawUI state = (if not $ view canMove state then (drawForm state:) else id)
    [viewport Viewport1 Vertical $ vBox $ makeBoxes state]

getAttrMap :: State -> AttrMap
getAttrMap state = attrMap defAttr [
    (attrName "current", if view canMove state then bg blue else defAttr),
    (attrName "searchBox", if not $ view canMove state then fg white else fg black),
    (attrName "focusedFormInputAttr", white `on` blue),
    (attrName "invalidFormInputAttr", red `on` yellow)]

wrap :: (Int -> Int) -> State -> State
wrap func state =
    let
        cur = func $ view cursorPos state
        top = fromIntegral  (length $ view mytems state) - 1
        wrapPoint = min top $ max 0 cur
    in
        set cursorPos wrapPoint state

checkCur :: Int -> Int -> Int -> Int
checkCur scroll cur height
    | diff > (adjustedHeight - scrollBarrier) = 3
    | diff < scrollBarrier = -3
    | otherwise = 0
    where
        scrollBarrier = 1
        diff = cur - (scroll `div` 3)
        adjustedHeight = height `div` 3

moveCursor :: (Int -> Int) -> EventM Name State ()
moveCursor func = do
    modify $ wrap func

    myViewport <- lookupViewport Viewport1
    let size = fromJust myViewport
    let height = regionHeight $ _vpSize size

    newPos <- use cursorPos
    scroll <- use cursorScroll
    let scrollby = checkCur scroll newPos height

    cursorScroll %= (+scrollby)
    vScrollBy (viewportScroll Viewport1) scrollby

movePage :: (Int -> Int) -> EventM Name State ()
movePage dir = do
    curQuery' <- use curQuery
    key' <- use key

    lastPage <- use page
    page %= dir
    page %= max 0
    page' <- use page
    when (lastPage /= page') (do
        next <- liftIO (getResponse curQuery' key' page')
        mytems .= next)

handleEvent :: BrickEvent Name Message -> EventM Name State ()
handleEvent (VtyEvent ev) = do
    curCanMove <- use canMove
    case (curCanMove, ev) of
        (_, V.EvKey V.KEsc []) -> halt
        (_, V.EvKey (V.KChar '=') []) -> do
            canMove %= not
            form .= emptyForm
        (_, V.EvKey (V.KChar 'q') []) -> halt

        (True, V.EvKey (V.KChar 'j') []) -> moveCursor (+1)
        (True, V.EvKey (V.KChar 'k') []) -> moveCursor (subtract 1)
        (True, V.EvKey (V.KChar 'h') []) -> movePage (subtract 1)
        (True, V.EvKey (V.KChar 'l') []) -> movePage (+ 1)
        (True, V.EvKey V.KEnter []) -> do
            curPos <- use cursorPos
            items <- use mytems
            curQuery .= link (items!!curPos)
            halt

        (False, V.EvKey V.KEnter []) -> do
            canMove .= True
            key' <- use key
            page' <- use page
            form' <- use form
            next <- liftIO (getResponse (unpack (_query $ formState form')) key' page')
            mytems .= next
        (False, _) -> zoom form $ handleFormEvent $ VtyEvent ev
        _ -> return ()

handleEvent _ = return ()


emptyForm :: Form Search e Name
emptyForm = mkForm Search {_query=""}

initialState :: String -> String -> State
initialState p_key p_query = State {
    _curQuery = p_query,
    _key = p_key,
    _page = 0,
    _mytems = [],
    _cursorPos = 0,
    _cursorScroll = 0,
    _canMove = True,
    _form = emptyForm}

start :: EventM Name State ()
start = do
    curQuery' <- use curQuery
    key' <- use key
    page' <- use page
    next <- liftIO (getResponse curQuery' key' page')
    mytems .= next

app :: App State Message Name
app = App {
    appStartEvent = start
    , appChooseCursor = focusRingCursor formFocus . view form
    , appDraw = drawUI
    , appHandleEvent = handleEvent
    , appAttrMap = getAttrMap
    }

