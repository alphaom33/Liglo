{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module HTMLBuilder where

import Debug.Trace
import qualified Data.List as L
import Data.List.Split
import Data.Char
import Data.Map (fromList, (!), (!?), keys, Map(..))
import Control.Monad (when)
import Data.Tree

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
import HTMLParser (Token(..), Tag, Attribute(..), _selfClosing, _opening, _tagName, tracer, _attrs)
import SearchApp (Name(..))
import Message
import CSSParser
import CSSTokenizer

-- data BuilderData = BuilderData {
--     _toRead :: [Token]
--     , _openTags :: [Tag]
--     , _out :: [Widget Name]
--     , _currentHBox :: [Widget Name]
--     , _currentText :: String
--     , _style :: Map String (Maybe (Widget Name) -> Maybe (Widget Name))
-- }
-- $(makeLenses ''BuilderData)
-- builderData emittedTokens style = BuilderData {_openTags=[], _out=[], _toRead=emittedTokens, _currentHBox=[], _currentText="", _style=style}
--
-- data State = State {
--     _emittedTokens :: [Token]
--     , _hotkey :: String
--     , _built :: [Widget Name]
--     , _css :: [ComponentValue]
-- }
-- $(makeLenses ''State)
--
-- hotkeys :: Map String (Int -> EventM Name State ())
-- hotkeys = fromList [
--     ("j", repeatify $ let vp = viewportScroll Viewport1 in vScrollBy vp (1))
--     , ("k", repeatify $ let vp = viewportScroll Viewport1 in vScrollBy vp (-1))
--     , ("u", repeatify $ let vp = viewportScroll Viewport1 in vScrollPage vp Up)
--     , ("d", repeatify $ let vp = viewportScroll Viewport1 in vScrollPage vp Down)
--     , ("gg", \ n -> do
--         let vp = viewportScroll Viewport1 in vScrollToBeginning vp
--         let vp = viewportScroll Viewport1 in vScrollBy vp n)
--     , ("G", discard $ let vp = viewportScroll Viewport1 in vScrollToEnd vp)
--     , ("q", discard halt)
--     ]
--     where 
--         discard e _ = e
--
--         repeatify e 0 = e
--         repeatify e num = do
--             e
--             repeatify e (num - 1)
--
-- makeWidget :: Tag -> BuilderData -> Maybe (Widget Name)
-- makeWidget t mhm = _makeWidget t mhm $ Just $ (str $ reverse $ _currentText mhm)
--
-- _makeWidget :: Tag -> BuilderData -> Maybe (Widget Name) -> Maybe (Widget Name)
-- _makeWidget t mhm out =
--     ((case getAttr "id" of
--         (Just (Attribute (_, id))) -> applyStyle ('#' : id)
--         Nothing -> id)
--     . (case getAttr "class" of
--         (Just (Attribute (_, classy))) -> \ out -> foldr (\ out thing -> applyStyle ('.' : out) thing) out $ splitOn " " classy
--         Nothing -> id)
--     . (applyStyle (_tagName t)))
--     out
--     where
--         getAttr :: String -> Maybe Attribute
--         getAttr name = (filter (\ (Attribute (n, _)) -> n == name) (_attrs t)) L.!? 0
--
--         applyStyle :: String -> Maybe (Widget Name) -> Maybe (Widget Name)
--         applyStyle name out = case _style mhm !? name of
--             (Just styley) -> styley out
--             Nothing -> out
--
-- buildHtml :: State -> [Widget Name]
-- buildHtml state = [viewport Viewport1 Vertical $ vBox $ _built state]
--
-- _buildHtml :: BuilderData -> BuilderData
--
-- _buildHtml mhm = case _toRead mhm of
--     (TagToken t:emittedTokens) -> _buildHtml $ (if
--         | _selfClosing t -> if _tagName t == "br"
--             then appendHbox mhm'
--             else mhm'
--         | _opening t -> over openTags (t :) $ if not (null $ _currentText mhm)
--             then (endings t mhm')
--             else mhm'
--         | not $ _opening t -> over openTags (drop 1) $ endings (_openTags mhm' !! 0) mhm'
--         | otherwise -> mhm')
--         where
--             appendWidget t mhm = case makeWidget t mhm of
--                 Nothing -> mhm
--                 (Just w) -> over currentHBox (++[w]) $ killText mhm
--             endings t mhm = if _tagName t `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div"]
--                 then appendHbox $ appendWidget t mhm
--                 else appendWidget t mhm
--
--     (Character c:emittedTokens) -> _buildHtml $ (if c `elem` " \n\t" && fmap _tagName (_openTags mhm' L.!? 0) /= Just "pre"
--         then killWhitespace
--         else if not (null (_openTags mhm)) && null (["head", "meta", "link", "script", "style", "select"] `L.intersect` map _tagName (_openTags mhm))
--             then over currentText (c:)
--             else id) mhm'
--     (e:emittedTokens) -> _buildHtml mhm'
--     [] -> appendHbox mhm
--     where 
--         (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)
--
--         appendHbox mhm = over out (++[hBox $ _currentHBox mhm]) $ set currentHBox [] $ killText mhm
--         killText = set currentText ""
--
-- killWhitespace mhm = _killWhitespace $ (if null (_out mhm) || (null (_currentText mhm) && null (_currentHBox mhm))
--     then id
--     else over currentText (' ' :)) mhm
--
-- _killWhitespace mhm = case next of
--     (Character c) -> if c `elem` " \n\t"
--         then killWhitespace mhm'
--         else mhm
--     _ -> mhm
--     where (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)
--
--
-- initialState :: [ComponentValue] -> [Token] -> State
-- initialState p_css p_emittedTokens  = State {
--     _emittedTokens = p_emittedTokens
--     , _hotkey = ""
--     , _built = _out $ _buildHtml $ builderData p_emittedTokens $ fromList $ _buildCSSMap p_css []
--     , _css = p_css
-- }
--
-- start :: EventM Name State ()
-- start = return ()
--
-- handler :: BrickEvent Name Message -> EventM Name State ()
--
-- handler (VtyEvent (EvKey k m)) = case k of
--     (KChar c) -> do
--         hotkey %= (((if MShift `elem` m then toUpper else id) c):)
--         _hotkey <- use hotkey
--         let (num, exec) = splitNum $ reverse _hotkey
--         when (exec `elem` (keys hotkeys)) (do
--             hotkey .= ""
--             (hotkeys ! exec) $ num)
--     KEsc -> hotkey .= ""
--     _ -> return ()
--     where
--         splitNum :: String -> (Int, String)
--         splitNum str = let (num, exec) = go str in if null num
--             then (0, exec)
--             else (read num - 1, exec)
--             where
--                 go [] = ("", "")
--                 go (c:cs) = if c `elem` "0123456789"
--                     then let (one, two) = go cs in (c : one, two)
--                     else ("", c:cs)
--
-- handler _ = return ()
--
-- parseColor v = case v of
--     [(PreservedValue (HashToken (_, n)))] -> 
--         let 
--             r = read $ "0x" ++ (take 2 n)
--             g = read $ "0x" ++ (take 2 $ drop 2 n)
--             b = read $ "0x" ++ (drop 4 n)
--         in RGBColor r g b
--     _ -> black
--
-- parseDeclarations :: [ComponentValue] -> Attr -> Maybe Attr
-- parseDeclarations ds out = case ds of
--     [] -> Just out
--     (nextDeclaration : rest) -> case nextDeclaration of
--         (DeclarationValue (Declaration n v _)) -> case n of
--             "background-color" -> go $ out `withBackColor` (parseColor v)
--             "background" -> go $ out `withBackColor` (parseColor v)
--             "color" -> go $ out `withForeColor` (parseColor v)
--             _ -> go out
--             where go = parseDeclarations rest
--
-- buildCSS :: State -> AttrMap
-- buildCSS state = attrMap defAttr $ _buildCSS (_css state) [
--     (attrName "b", defAttr `withStyle` V.bold `withBackColor` black)
--     , (attrName "tt", defAttr `withStyle` V.italic)
--     ]
--
-- _buildCSS :: [ComponentValue] -> [(AttrName, Attr)] -> [(AttrName, Attr)]
-- _buildCSS css out = case css of
--     [] -> out
--     (nextValue : rest) -> _buildCSS rest $ case nextValue of
--         (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ns ds))) -> 
--             let name = case ns of
--                     [(PreservedValue v)] -> case v of
--                         (IdentToken n) -> n
--                         (HashToken (_, n)) -> '#' : n
--                         _ -> ""
--                     _ -> ""
--                 declrs = parseDeclarations ds defAttr
--             in if name == ""
--                 then out
--                 else case declrs of
--                     Nothing -> out
--                     (Just decs) -> (attrName name, decs) : out
--         _ -> out


parseDecleretiens :: [ComponentValue] -> CSSAttribute -> CSSAttribute
parseDecleretiens ds out = case ds of
    [] -> out
    (nextDeclaration : rest) -> parseDecleretiens rest $ case nextDeclaration of
        (DeclarationValue (Declaration n vs _)) -> case n of
            "position" -> case vs of
                [(PreservedValue (IdentToken "absolute"))] -> out . (\ _ -> Nothing)
                _ -> out . (\ _ -> Nothing)
            _ -> out
--
-- _buildCSSMap :: [ComponentValue] -> [(String, Maybe (Widget Name) -> Maybe (Widget Name))] -> [(String, Maybe (Widget Name) -> Maybe (Widget Name))]
-- _buildCSSMap css out = case css of
--     [] -> out
--     (nextValue : rest) -> _buildCSSMap rest $ case nextValue of
--         (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ns ds))) ->
--             let
--                 name = case ns of
--                     [(PreservedValue v)] -> case v of
--                         (IdentToken n) -> n
--                         (HashToken (_, n)) -> '#' : n
--                         _ -> ""
--                     _ -> ""
--                 declrs = parseDecleretiens ds (fmap $ withAttr (attrName name))
--             in if name == ""
--                 then out
--                 else (name, declrs) : out
--         _ -> out
--     where
--         -- addToTree locators val tree = case locators of
--         --     [] -> tree
--         --     (locator : rest) -> 

type CSSAttribute = Maybe (Widget Name) -> Maybe (Widget Name)
type PreSelectorNode = (Selector, [ComponentValue])
type SelectorNode = ([Selector], [ComponentValue])

buildCSSTree :: [ComponentValue] -> Tree (Selector, CSSAttribute)
buildCSSTree css = unfoldTree _buildCSSTree (blamCSS [] css)
    where
        blamCSS :: [SelectorNode] -> [ComponentValue] -> (PreSelectorNode, [SelectorNode])
        blamCSS out css = case css of
            [] -> 
                let 
                    maybeStar = filter ((== [StarSelector]) . fst) out
                    maybeStarless = filter ((/= [StarSelector]) . fst) out
                in 
                    if not $ null maybeStar
                        then ((StarSelector, snd $ maybeStar !! 0), maybeStarless)
                        else ((StarSelector, []), out)
            (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ss ds)) : rest) -> blamCSS (foldr (\ s b -> (s, ds) : b) [] ss ++ out) rest

_buildCSSTree :: (PreSelectorNode, [SelectorNode]) -> ((Selector, CSSAttribute), [(PreSelectorNode, [SelectorNode])])
_buildCSSTree ((selector, val), css) =
    let 
        selectish :: SelectorNode -> [(PreSelectorNode, [SelectorNode])] -> [(PreSelectorNode, [SelectorNode])]
        selectish (selector : rest, vals) out =
            let
                filtered = filter ((== selector) . fst . fst) out
                ((_, add), left) = filtered !! 0
                unfiltered = filter ((/= selector) . fst . fst) out
                vals' = if null rest then vals else []

                addendum remainder = if null rest
                    then remainder
                    else (rest, vals) : remainder
            in 
                if not $ null filtered
                    then ((selector, add ++ vals'), addendum left) : unfiltered
                    else ((selector, vals'), addendum []) : out

        sorted :: [(PreSelectorNode, [SelectorNode])]
        sorted = foldr selectish [] css
    in ((selector, parseDecleretiens val id), sorted)

countCSSTree :: Int -> Tree (Selector, CSSAttribute) -> Int
countCSSTree out tree = case subForest tree of
    [] -> 1
    _ -> foldr ((+) . (countCSSTree 0)) out (subForest tree)

-- app :: App State Message Name
-- app = App {
--     appStartEvent = start
--     , appChooseCursor = neverShowCursor
--     , appDraw = buildHtml
--     , appHandleEvent = handler
--     , appAttrMap = buildCSS
--     }

