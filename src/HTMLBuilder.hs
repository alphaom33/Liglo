{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HTMLBuilder where

import qualified Data.List as L
import Data.List.Split
import Data.Tree

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

import HTMLParser (Token(..), Tag, Attribute(..), _selfClosing, _opening, _tagName, tracer, _attrs)
import CSSParser
import CSSTokenizer
import qualified Mortar

type CSSAttribute = Maybe String -> Maybe String
type PreSelectorNode = (Selector, [ComponentValue])
type SelectorNode = ([Selector], [ComponentValue])

data BuilderData = BuilderData {
    _toRead :: [Token]
    , _openTags :: [Tag]
    , _out :: [String]
    , _currentText :: String
    , _style :: Tree (Selector, CSSAttribute)
    , _lined :: Bool
}
$(makeLenses ''BuilderData)

builderData :: [Token] -> Tree (Selector, CSSAttribute) -> BuilderData
builderData emittedTokens p_style = BuilderData {_openTags=[], _toRead=emittedTokens, _out=[], _currentText="", _style=p_style, _lined=True}

makeWidget :: BuilderData -> CSSAttribute
makeWidget mhm = applyStyle (_openTags mhm) (_style mhm) id

getAttr :: Tag -> String -> Maybe Attribute
getAttr t name = filter (\ (Attribute (n, _)) -> n == name) (_attrs t) L.!? 0

applyStyle :: [Tag] -> Tree (Selector, CSSAttribute) -> CSSAttribute -> CSSAttribute
applyStyle [] _ attribute = attribute
applyStyle (tag:tags) (Node (selector, value) children) attribute = case selector of
    StarSelector -> foldr (applyStyle (tag:tags)) (value . attribute) children
    (TagSelector n) -> checkStyleApply n (_tagName tag) attribute
    (HashSelector n) -> checkStyleApply n (maybe "" (\ (Attribute (_, v)) -> v) $ getAttr tag "id") attribute
    (ClassSelector n) ->
        let classes = splitOn " " $ maybe "" (\ (Attribute (_, v)) -> v) $ getAttr tag "style"
        in foldr (checkStyleApply n) attribute classes
    (StateSelector _ _) -> attribute
    where
        checkStyleApply check val currentAttribute = if check == val
            then foldr (applyStyle tags) (value . currentAttribute) children
            else attribute

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:_) -> _buildHtml (if -- ...what
        | _selfClosing t -> if _tagName t == "br"
            then appendHbox mhm'
            else mhm'
        | _opening t -> 
            over openTags (t:) 
            . over out ([]:)
            . (if not . null . _currentText $ mhm
                then killText . putText (reverse $ _currentText mhm)
                else id)
            $ mhm'
        | not $ _opening t -> 
            over openTags (drop 1) 
            . endings (head $ _openTags mhm')
            . killText
            . putText (reverse $ _currentText mhm')
            $ mhm'
        | otherwise -> mhm')
        where
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
killWhitespace mhm = _killWhitespace $ (if not $ _lined mhm
    then over currentText (' ' :)
    else id) mhm

_killWhitespace :: BuilderData -> BuilderData
_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (head $ _toRead mhm, over toRead (drop 1) mhm)


parseColor :: [ComponentValue] -> (Int, Int, Int)
parseColor v = case v of
    [PreservedValue (HashToken (_, n))] -> 
        let 
            r = read $ "0x" ++ take 2 n
            g = read $ "0x" ++ take 2 (drop 2 n)
            b = read $ "0x" ++ drop 4 n
        in (r, g, b)
    [PreservedValue (IdentToken n)] -> case n of
        "white" -> (255, 255, 255)
        "black" -> (0, 0, 0)

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
        (DeclarationValue (Declaration n vs _)) -> case n of
            "position" -> case vs of
                [PreservedValue (IdentToken "absolute")] -> const Nothing . out
                _ -> out
            "color" -> setColor vs Mortar.surroundForegroundColor
            "background" -> setColor vs Mortar.surroundBackgroundColor
            "font-weight" -> case vs of
                [PreservedValue (IdentToken "bold")] -> fmap Mortar.surroundBold
            "font-style" -> case vs of
                [PreservedValue (IdentToken "italic")] -> fmap Mortar.surroundItalic
                [PreservedValue (IdentToken "normal")] -> fmap (Mortar.resetItalic++)
            _ -> out
    where
        setColor vs func = 
            let (r, g, b) = parseColor vs
            in out . fmap (func r g b)

buildCSSTree :: [ComponentValue] -> Tree (Selector, CSSAttribute)
buildCSSTree css = unfoldTree _buildCSSTree (blamCSS [] css)

blamCSS :: [SelectorNode] -> [ComponentValue] -> (PreSelectorNode, [SelectorNode])
blamCSS collapsed css = case css of
    [] -> 
        let 
            maybeStar = filter ((== [StarSelector]) . fst) collapsed
            maybeStarless = filter ((/= [StarSelector]) . fst) collapsed
        in 
            if not $ null maybeStar
                then ((StarSelector, snd $ head maybeStar), maybeStarless)
                else ((StarSelector, []), collapsed)
    (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ss ds)) : rest) -> blamCSS (map (\ s -> (reverse s, ds)) ss ++ collapsed) rest

_buildCSSTree :: (PreSelectorNode, [SelectorNode]) -> ((Selector, CSSAttribute), [(PreSelectorNode, [SelectorNode])])
_buildCSSTree ((selector, val), css) =
    let 
        selectish :: SelectorNode -> [(PreSelectorNode, [SelectorNode])] -> [(PreSelectorNode, [SelectorNode])]
        selectish (currentSelector : rest, vals) children =
            let
                filtered = filter ((== currentSelector) . fst . fst) children
                ((_, add), left) = head filtered
                unfiltered = filter ((/= currentSelector) . fst . fst) children
                vals' = if null rest then vals else []

                addendum remainder = if null rest
                    then remainder
                    else (rest, vals) : remainder
            in 
                if not $ null filtered
                    then ((currentSelector, add ++ vals'), addendum left) : unfiltered
                    else ((currentSelector, vals'), addendum []) : children

        sorted :: [(PreSelectorNode, [SelectorNode])]
        sorted = foldr selectish [] css
    in ((selector, parseDecleretiens val id), sorted)

countCSSTree :: Int -> Tree (Selector, CSSAttribute) -> Int
countCSSTree num tree = case subForest tree of
    [] -> 1
    _ -> foldr ((+) . countCSSTree 0) num (subForest tree)

parseWebpage :: [Token] -> [ComponentValue] -> String
parseWebpage emittedTokens css = head $ _out $ _buildHtml $ builderData emittedTokens $ buildCSSTree css

drawCSSTree :: [ComponentValue] -> String
drawCSSTree css = drawTree $ show . fst <$> buildCSSTree css
