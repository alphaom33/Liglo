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
builderData emittedTokens style = BuilderData {_openTags=[], _toRead=emittedTokens, _out=[], _currentText="", _style=style, _lined=True}

makeWidget :: BuilderData -> CSSAttribute
makeWidget mhm = applyStyle (_openTags mhm) (_style mhm) id

getAttr :: Tag -> String -> Maybe Attribute
getAttr t name = (filter (\ (Attribute (n, _)) -> n == name) (_attrs t)) L.!? 0

applyStyle :: [Tag] -> Tree (Selector, CSSAttribute) -> CSSAttribute -> CSSAttribute
applyStyle [] css out = out
applyStyle (tag:tags) (Node (selector, value) children) out = case selector of
    StarSelector -> foldr (applyStyle (tag:tags)) (value . out) children
    (TagSelector n) -> checkStyleApply n (_tagName tag) out
    (HashSelector n) -> checkStyleApply n (fromMaybe "" $ fmap (\ (Attribute (_, v)) -> v) $ getAttr tag "id") out
    (ClassSelector n) ->
        let classes = splitOn " " $ fromMaybe "" $ fmap (\ (Attribute (_, v)) -> v) $ getAttr tag "style"
        in foldr (checkStyleApply n) out classes
    (StateSelector n1 n2) -> out
    where
        checkStyleApply check val out = if check == val
            then foldr (applyStyle tags) (value . out) children
            else out

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:emittedTokens) -> _buildHtml $ (if -- ...what
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
            . endings (_openTags mhm' !! 0)
            . killText
            . putText (reverse $ _currentText mhm')
            $ mhm'
        | otherwise -> mhm')
        where
            appendWidget mhm = case makeWidget mhm $ Just $ (_out mhm !! 0) of
                Nothing -> mhm'
                (Just w) -> putText w $ killText mhm'
                where mhm' = over out (drop 1) mhm
            endings t mhm = if _tagName t `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div"]
                then appendHbox $ appendWidget mhm
                else appendWidget mhm

    (Character c:emittedTokens) -> _buildHtml $ (if c `elem` " \n\t" && fmap _tagName (_openTags mhm' L.!? 0) /= Just "pre"
        then killWhitespace
        else if not (null (_openTags mhm)) && null (["head", "meta", "link", "script", "style", "select"] `L.intersect` map _tagName (_openTags mhm))
            then (over currentText (c:)) . (set lined False)
            else id) mhm'

    (e:emittedTokens) -> _buildHtml mhm'

    [] -> appendHbox mhm

    where 
        (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)

        putText text = over out (\ out -> case out of
            [] -> [text]
            (current:rest) -> (current ++ text) : rest)

        appendHbox mhm = set lined True $ putText "\n" $ killText mhm
        killText = set currentText ""

killWhitespace mhm = _killWhitespace $ (if not $ _lined mhm
    then over currentText (' ' :)
    else id) mhm

_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (_toRead mhm !! 0, over toRead (drop 1) mhm)


parseColor :: [ComponentValue] -> (Int, Int, Int)
parseColor v = case v of
    [(PreservedValue (HashToken (_, n)))] -> 
        let 
            r = read $ "0x" ++ (take 2 n)
            g = read $ "0x" ++ (take 2 $ drop 2 n)
            b = read $ "0x" ++ (drop 4 n)
        in (r, g, b)
    [(PreservedValue (IdentToken n))] -> case n of
        "white" -> (255, 255, 255)
        "black" -> (0, 0, 0)

selectorToString selector = case selector of
    StarSelector -> ""
    (HashSelector n) -> '#' : n
    (ClassSelector n) -> '.' : n
    (TagSelector n) -> n
    (StateSelector n1 n2) -> ""

parseDecleretiens :: [ComponentValue] -> CSSAttribute -> CSSAttribute
parseDecleretiens ds out = case ds of
    [] -> out
    (nextDeclaration : rest) -> parseDecleretiens rest $ case nextDeclaration of
        (DeclarationValue (Declaration n vs _)) -> case n of
            "position" -> case vs of
                [(PreservedValue (IdentToken "absolute"))] -> (\ _ -> Nothing) . out
                _ -> out
            "color" -> setColor vs Mortar.surroundForegroundColor
            "background" -> setColor vs Mortar.surroundBackgroundColor
            "font-weight" -> case vs of
                [(PreservedValue (IdentToken "bold"))] -> fmap Mortar.surroundBold
            "font-style" -> case vs of
                [(PreservedValue (IdentToken "italic"))] -> fmap Mortar.surroundItalic
            _ -> out
    where
        setColor vs func = 
            let (r, g, b) = parseColor vs
            in out . (fmap (func r g b))

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
            (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ss ds)) : rest) -> blamCSS (foldr (\ s b -> (reverse s, ds) : b) [] ss ++ out) rest

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

parseWebpage :: [Token] -> [ComponentValue] -> String
parseWebpage emittedTokens css = (!! 0) $ _out $ _buildHtml $ builderData emittedTokens $ buildCSSTree css

drawCSSTree css = drawTree $ fmap (show . fst) $ buildCSSTree css
