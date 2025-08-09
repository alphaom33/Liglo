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
import Data.Maybe

import Brick
import Graphics.Vty as V

import Text.Wrap

import Lens.Micro.Mtl ((.=), (%=), use, view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

import CharacterReferences
import HTMLParser (Token(..), Tag, Attribute(..), _selfClosing, _opening, _tagName, tracer, _attrs)
import SearchApp (Name(..))
import Message
import CSSParser
import CSSTokenizer

type CSSAttribute = Maybe String -> Maybe String
type PreSelectorNode = (Selector, [ComponentValue])
type SelectorNode = ([Selector], [ComponentValue])

data BuilderData = BuilderData {
    _toRead :: [Token]
    , _openTags :: [Tag]
    , _out :: String
    , _currentText :: String
    , _style :: Tree (Selector, CSSAttribute)
}
$(makeLenses ''BuilderData)
builderData emittedTokens style = BuilderData {_openTags=[], _toRead=emittedTokens, _out="", _currentText="", _style=style}

data State = State {
    _emittedTokens :: [Token]
    , _hotkey :: String
    , _built :: String
    , _css :: [ComponentValue]
}
$(makeLenses ''State)

hotkeys :: Map String (Int -> EventM Name State ())
hotkeys = fromList [
    ("j", repeatify $ let vp = viewportScroll Viewport1 in vScrollBy vp (1))
    , ("k", repeatify $ let vp = viewportScroll Viewport1 in vScrollBy vp (-1))
    , ("u", repeatify $ let vp = viewportScroll Viewport1 in vScrollPage vp Up)
    , ("d", repeatify $ let vp = viewportScroll Viewport1 in vScrollPage vp Down)
    , ("gg", \ n -> do
        let vp = viewportScroll Viewport1 in vScrollToBeginning vp
        let vp = viewportScroll Viewport1 in vScrollBy vp n)
    , ("G", discard $ let vp = viewportScroll Viewport1 in vScrollToEnd vp)
    , ("q", discard halt)
    ]
    where 
        discard e _ = e

        repeatify e 0 = e
        repeatify e num = do
            e
            repeatify e (num - 1)

makeWidget :: Tag -> BuilderData -> Maybe String
makeWidget t mhm = _makeWidget t mhm $ Just $ reverse $ _currentText mhm

_makeWidget :: Tag -> BuilderData -> Maybe String -> Maybe String
_makeWidget t mhm out = applyStyle "" (_openTags mhm) (_style mhm) out
    where
        getAttr :: String -> Maybe Attribute
        getAttr name = (filter (\ (Attribute (n, _)) -> n == name) (_attrs t)) L.!? 0

        applyStyle :: String -> [Tag] -> Tree (Selector, CSSAttribute) -> Maybe String -> Maybe String
        applyStyle depth [] css out = out
        applyStyle depth (tag : tags) (Node (selector, value) children) out = case selector of
            StarSelector -> foldr (applyStyle "" (tag:tags)) (value out) children
            (TagSelector n) -> checkStyleApply n n (_tagName tag) out
            (HashSelector n) -> checkStyleApply ('#':n) n (fromMaybe "" $ fmap (\ (Attribute (_, v)) -> v) $ getAttr "id") out
            (ClassSelector n) ->
                let classes = splitOn " " $ fromMaybe "" $ fmap (\ (Attribute (_, v)) -> v) $ getAttr "style"
                in foldr (checkStyleApply ('.':n) n) out classes
            (StateSelector n1 n2) -> out
            where
                checkStyleApply prefix check val out = if check == val
                    then foldr (applyStyle (depth ++ prefix) tags) (value out) children
                    else out

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:emittedTokens) -> _buildHtml $ (if
        | _selfClosing t -> if _tagName t == "br"
            then appendHbox mhm'
            else mhm'
        | _opening t -> over openTags (t :) $ if not (null $ _currentText mhm)
            then (endings t mhm')
            else mhm'
        | not $ _opening t -> over openTags (drop 1) $ endings (_openTags mhm' !! 0) mhm'
        | otherwise -> mhm')
        where
            appendWidget t mhm = case makeWidget t mhm of
                Nothing -> mhm
                (Just w) -> over out (++w) $ killText mhm
            endings t mhm = if _tagName t `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div"]
                then appendHbox $ appendWidget t mhm
                else appendWidget t mhm

    (Character c:emittedTokens) -> _buildHtml $ (if c `elem` " \n\t" && fmap _tagName (_openTags mhm' L.!? 0) /= Just "pre"
        then killWhitespace
        else if not (null (_openTags mhm)) && null (["head", "meta", "link", "script", "style", "select"] `L.intersect` map _tagName (_openTags mhm))
            then over currentText (c:)
            else id) mhm'
    (e:emittedTokens) -> _buildHtml mhm'
    [] -> appendHbox mhm
    where 
        (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)

        appendHbox mhm = over out (++"\n") $ killText mhm
        killText = set currentText ""

killWhitespace mhm = _killWhitespace $ (if null (_out mhm) || null (_currentText mhm)
    then id
    else over currentText (' ' :)) mhm

_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)


initialState :: [ComponentValue] -> [Token] -> State
initialState p_css p_emittedTokens  = State {
    _emittedTokens = p_emittedTokens
    , _hotkey = ""
    , _built = _out $ _buildHtml $ builderData p_emittedTokens $ buildCSSTree p_css
    , _css = p_css
}

start :: EventM Name State ()
start = return ()

handler :: BrickEvent Name Message -> EventM Name State ()

handler (VtyEvent (EvKey k m)) = case k of
    (KChar c) -> do
        hotkey %= (((if MShift `elem` m then toUpper else id) c):)
        _hotkey <- use hotkey
        let (num, exec) = splitNum $ reverse _hotkey
        when (exec `elem` (keys hotkeys)) (do
            hotkey .= ""
            (hotkeys ! exec) $ num)
    KEsc -> hotkey .= ""
    _ -> return ()
    where
        splitNum :: String -> (Int, String)
        splitNum str = let (num, exec) = go str in if null num
            then (0, exec)
            else (read num - 1, exec)
            where
                go [] = ("", "")
                go (c:cs) = if c `elem` "0123456789"
                    then let (one, two) = go cs in (c : one, two)
                    else ("", c:cs)

handler _ = return ()

parseColor v = case v of
    [(PreservedValue (HashToken (_, n)))] -> 
        let 
            r = read $ "0x" ++ (take 2 n)
            g = read $ "0x" ++ (take 2 $ drop 2 n)
            b = read $ "0x" ++ (drop 4 n)
        in RGBColor r g b
    _ -> black

parseDeclarations :: [ComponentValue] -> Attr -> Maybe Attr
parseDeclarations ds out = case ds of
    [] -> Just out
    (nextDeclaration : rest) -> case nextDeclaration of
        (DeclarationValue (Declaration n v _)) -> case n of
            "background-color" -> go $ out `withBackColor` (parseColor v)
            "background" -> go $ out `withBackColor` (parseColor v)
            "color" -> go $ out `withForeColor` (parseColor v)
            _ -> go out
            where go = parseDeclarations rest

selectorToString selector = case selector of
    StarSelector -> ""
    (HashSelector n) -> '#' : n
    (ClassSelector n) -> '.' : n
    (TagSelector n) -> n
    (StateSelector n1 n2) -> ""

buildCSS :: State -> AttrMap
buildCSS state = attrMap defAttr $ _buildCSS (_css state) [
    (attrName "b", defAttr `withStyle` V.bold `withBackColor` black)
    , (attrName "tt", defAttr `withStyle` V.italic)
    ]

_buildCSS :: [ComponentValue] -> [(AttrName, Attr)] -> [(AttrName, Attr)]
_buildCSS css out = case css of
    [] -> out
    (nextValue : rest) -> _buildCSS rest $ case nextValue of
        (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ns ds))) -> 
            let names = map (foldr ((++) . selectorToString) "") ns
                declrs = parseDeclarations ds defAttr
            in case declrs of
                Nothing -> out
                (Just decs) -> map (\ name -> (attrName name, decs)) names ++ out
        _ -> out

parseDecleretiens :: [ComponentValue] -> CSSAttribute -> CSSAttribute
parseDecleretiens ds out = case ds of
    [] -> out
    (nextDeclaration : rest) -> parseDecleretiens rest $ case nextDeclaration of
        (DeclarationValue (Declaration n vs _)) -> case n of
            "position" -> case vs of
                [(PreservedValue (IdentToken "absolute"))] -> out . (\ _ -> Nothing)
                _ -> out . (\ _ -> Nothing)
            _ -> out

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
