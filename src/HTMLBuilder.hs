{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HTMLBuilder where

import qualified Data.List as L
import qualified Data.Map as M
import Data.List.Split
import Data.Tree

import Debug.Trace

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

import HTMLParser (Token(..), Tag, Attribute(..), _selfClosing, _opening, _tagName, tracer, _attrs, opening)
import CSSParser
import CSSTokenizer
import qualified Mortar

import CharacterReferences

type CSSAttribute = Maybe String -> Maybe String
instance Show CSSAttribute where
    show _ = "<attr>"


type PreSelectorNode = (SelectorData, [ComponentValue])
type SelectorNode = ([SelectorData], [ComponentValue])

data BuilderData = BuilderData {
    _toRead :: [Token]
    , _openTags :: [Tag]
    , _out :: [String]
    , _currentText :: String
    , _style :: Tree (SelectorData, CSSAttribute)
    , _lined :: Bool
} deriving Show
$(makeLenses ''BuilderData)

builderData :: [Token] -> Tree (SelectorData, CSSAttribute) -> BuilderData
builderData emittedTokens p_style = BuilderData {_openTags=[], _toRead=emittedTokens, _out=[], _currentText="", _style=p_style, _lined=True}

makeWidget :: BuilderData -> CSSAttribute
makeWidget mhm = applyStyle (_openTags mhm) (_style mhm) id

getAttr :: Tag -> String -> Maybe Attribute
getAttr t name = filter (\ (Attribute (n, _)) -> n == name) (_attrs t) L.!? 0


checkSelector tag selector = case selector of
    (TagSelector n) -> n == _tagName tag
    (HashSelector n) -> n == getAttrJust tag "id"
    (ClassSelector n) ->
        let classes = splitOn " " $ getAttrJust tag "class"
        in elem n classes
    (AttrSelector a c) -> c (getAttrValue <$> getAttr tag a)
    (StateSelector _ _) -> False

getAttrJust tag = maybe "" getAttrValue . getAttr tag
getAttrValue (Attribute (_, v)) = v

applyStyle :: [Tag] -> Tree (SelectorData, CSSAttribute) -> CSSAttribute -> CSSAttribute
applyStyle [] _ attribute = attribute
applyStyle (tag:tags) (Node ((combinator, selector), value) children) attribute = case selector of
    StarSelector -> toNext (tag:tags) attribute
    (ClassSelector n) ->
        let classes = splitOn " " $ getAttrJust tag "class"
        in foldr (checkStyleApply n) attribute classes
    _ -> case combinator of
        CurrentCombinator -> if checkSelector tag selector
            then toNext (tag:tags) attribute
            else attribute
        ChildCombinator -> if checkSelector tag selector
            then toNext tags attribute
            else attribute
        -- DescendantCombinator -> case go (tag:tags) of
        --     [] -> attribute
        --     rest -> toNext rest attribute
        where
            go [] = []
            go (tag:tags) = if checkSelector tag selector
                then tags
                else go tags
    where
        toNext tags currentAttribute = foldr (applyStyle tags) (value . currentAttribute) children

        checkStyleApply check val currentAttribute = if check == val
            then toNext tags currentAttribute
            else attribute

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:_) -> _buildHtml (if -- ...what
        | _selfClosing t -> (if _tagName t == "br"
            then appendHbox
            else id) $ checkSet mhm'
        | _opening t ->
            over openTags (t:)
            . over out ([]:)
            . checkSet
            $ mhm'
        | not $ _opening t -> killExtras mhm'
        | otherwise -> mhm')
        where
            checkSet = if not . null . _currentText $ mhm
                then killText . putText (reverse $ _currentText mhm)
                else id

            killExtras mhm''
                | null $ _openTags mhm'' = mhm''
                | (_tagName . head . _openTags $ mhm') /= _tagName t =
                    killExtras
                    . doEnd
                    $ mhm''
                | otherwise = doEnd mhm''

            doEnd mhm'' =
                over openTags (drop 1)
                . endings (head $ _openTags mhm'')
                . killText
                . putText (reverse $ _currentText mhm'')
                $ mhm''

            appendWidget state = case makeWidget state $ Just $ head $ _out state of
                Nothing -> state'
                (Just w) -> putText w $ killText state'
                where state' = over out (drop 1) state

            endings tag state = if _tagName tag `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div"]
                then appendWidget $ appendHbox state
                else appendWidget state

    (Character c:_) -> _buildHtml $ (if c `elem` " \n\t" && fmap _tagName (_openTags mhm' L.!? 0) /= Just "pre"
        then killWhitespace
        else if not (null (_openTags mhm)) && null (["head", "meta", "link", "script", "style", "select"] `L.intersect` map _tagName (_openTags mhm))
            then over currentText (c:) . set lined False
            else id) mhm'

    (_:_) -> _buildHtml mhm'

    [] -> appendHbox mhm
    where
        mhm' = over toRead (drop 1) mhm

        putText text = over out (\case
            [] -> [text]
            (current:rest) -> (current ++ text) : rest)

        appendHbox = set lined True . putText "\n" . killText
        killText = set currentText ""

killWhitespace :: BuilderData -> BuilderData
killWhitespace mhm = _killWhitespace $ (if _lined mhm
    then id
    else over currentText (' ' :)) mhm

_killWhitespace :: BuilderData -> BuilderData
_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then _killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (head $ _toRead mhm, over toRead (drop 1) mhm)

parseFuncValue = filter (/= PreservedValue CommaToken)

parseColor :: [ComponentValue] -> (Int, Int, Int)
parseColor v = case v of
    [FunctionValue ("rgb", comps)] -> case parseFuncValue comps of
        [PreservedValue (NumberToken r), PreservedValue (NumberToken g), PreservedValue (NumberToken b)] -> (round r, round g, round b)
    [FunctionValue ("hsl", comps)] -> case parseFuncValue comps of
        [PreservedValue (NumberToken h), PreservedValue (NumberToken s), PreservedValue (NumberToken l)] -> overColor round $ hslToRgb (h, s, l)
    [PreservedValue (HashToken (_, n))] ->
        let
            enned = if length n == 3
                then foldr (\ en ens -> en : en : ens) [] n
                else n
            r = read $ "0x" ++ take 2 enned
            g = read $ "0x" ++ take 2 (drop 2 enned)
            b = read $ "0x" ++ drop 4 enned
        in (r, g, b)
    [PreservedValue (IdentToken n)] -> colorNameToColor M.! n
    where
        overColor f (r, g, b) = (f r, f g, f b)

        hslToRgb :: (Float, Float, Float) -> (Float, Float, Float)
        hslToRgb (_h, s, l) = ((r' + m) * 255, (b' + m) * 255, (g' + m) * 255)
            where
                h = fromIntegral $ round _h `mod` 360
                c = (1 - abs (2 * l - 1)) * s
                x = c * (1 - abs (fromIntegral $ (round (h / 60) `mod` 2) - 1))
                m = l - c / 2
                (r', g', b')
                    | 0 <= h && h < 60 = (c, x, 0)
                    | 60 <= h && h < 120 = (x, c, 0)
                    | 120 <= h && h < 180 = (0, c, x)
                    | 180 <= h && h < 240 = (0, x, c)
                    | 240 <= h && h < 300 = (x, 0, c)
                    | 300 <= h && h < 360 = (c, 0, x)

selectorToString :: Selector -> String
selectorToString selector = case selector of
    StarSelector -> ""
    (HashSelector n) -> '#' : n
    (ClassSelector n) -> '.' : n
    (TagSelector n) -> n
    (StateSelector _ _) -> ""

parseDecleretiens :: [ComponentValue] -> CSSAttribute -> CSSAttribute
parseDecleretiens ds out = case ds of
    [] -> out
    (nextDeclaration : rest) -> parseDecleretiens rest $ case nextDeclaration of
        (DeclarationValue (Declaration n vs _)) -> out . case n of
            "position" -> case vs of
                [PreservedValue (IdentToken "relative")] -> id
                _ -> const Nothing
            "float" -> case vs of
                [PreservedValue (IdentToken "none")] -> id
                _ -> const Nothing
            "color" -> setColor vs Mortar.surroundForegroundColor
            "background" -> setColor vs Mortar.surroundBackgroundColor
            "background-color" -> setColor vs Mortar.surroundBackgroundColor
            "font-weight" -> case vs of
                [PreservedValue (IdentToken "bold")] -> fmap Mortar.surroundBold
            "font-style" -> case vs of
                [PreservedValue (IdentToken "italic")] -> fmap Mortar.surroundItalic
                [PreservedValue (IdentToken "normal")] -> fmap (Mortar.resetItalic++)
            "text-decoration" -> case vs of
                [PreservedValue (IdentToken "underline")] -> fmap Mortar.surroundUnderline
                [PreservedValue (IdentToken "line-through")] -> fmap Mortar.surroundStrikethrough
                [PreservedValue (IdentToken "none")] -> fmap ((Mortar.resetUnderline ++ Mortar.resetStrikethrough)++)
            _ -> out
    where
        setColor vs func =
            let (r, g, b) = parseColor vs
            in fmap (func r g b)

buildCSSTree :: [ComponentValue] -> Tree (SelectorData, CSSAttribute)
buildCSSTree css = unfoldTree _buildCSSTree (blamCSS [] css)

blamCSS :: [SelectorNode] -> [ComponentValue] -> (PreSelectorNode, [SelectorNode])
blamCSS collapsed css = case css of
    [] ->
        let
            (maybeStar, maybeStarless) = L.partition ((== [(CurrentCombinator, StarSelector)]) . fst) collapsed
        in
            if not $ null maybeStar
                then (((CurrentCombinator, StarSelector), snd $ head maybeStar), maybeStarless)
                else (((CurrentCombinator, StarSelector), []), collapsed)
    (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ss ds)) : rest) -> blamCSS (map (\ s -> (reverse s, ds)) ss ++ collapsed) rest
    (_:rest) -> blamCSS collapsed rest

_buildCSSTree :: (PreSelectorNode, [SelectorNode]) -> ((SelectorData, CSSAttribute), [(PreSelectorNode, [SelectorNode])])
_buildCSSTree ((selector, val), css) =
    let
        selectish :: SelectorNode -> [(PreSelectorNode, [SelectorNode])] -> [(PreSelectorNode, [SelectorNode])]
        selectish ([], _) children = children
        selectish (currentSelector : rest, vals) children =
            let
                (filtered, unfiltered) = L.partition ((== currentSelector) . fst . fst) children
                ((_, add), left) = head filtered
                vals' = if null rest then vals else []

                addendum remainder = if null rest
                    then remainder
                    else (rest, vals) : remainder
            in
                if null filtered
                    then ((currentSelector, vals'), addendum []) : children
                    else ((currentSelector, add ++ vals'), addendum left) : unfiltered

        sorted :: [(PreSelectorNode, [SelectorNode])]
        sorted = foldr selectish [] css
    in ((selector, parseDecleretiens val id), sorted)

countCSSTree :: Int -> Tree (SelectorData, CSSAttribute) -> Int
countCSSTree num tree = case subForest tree of
    [] -> 1
    _ -> foldr ((+) . countCSSTree 0) num (subForest tree)

parseWebpage :: [Token] -> [ComponentValue] -> String
parseWebpage emittedTokens css = head $ _out $ _buildHtml $ builderData emittedTokens $ buildCSSTree css

drawCSSTree :: [ComponentValue] -> String
drawCSSTree css = drawTree $ show . fst <$> buildCSSTree css
