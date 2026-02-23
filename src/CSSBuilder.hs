{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CSSBuilder where

import Data.Tree (Tree(Node), subForest, unfoldTree, drawTree)

import CSSParser
import CSSTokenizer (CSSToken(..))
import TextState
import CharacterReferences (colorNameToColor)

import Debug.Trace (trace)

import Lens.Micro (set)

import qualified Data.Map as M
import qualified Data.List as L
import HTMLParser (Tag (_tagName, _attrs), Attribute(..))
import Data.List.Split (splitOn)

type CSSAttribute = TextState -> TextState
instance Show CSSAttribute where
    show _ = "<attr>"

type PreSelectorNode = (SelectorData, [ComponentValue])
type SelectorNode = ([SelectorData], [ComponentValue])
type CSSTree = Tree (SelectorData, CSSAttribute)

parseFuncValue :: [ComponentValue] -> [ComponentValue]
parseFuncValue = filter (/= PreservedValue CommaToken)

parseColor :: [ComponentValue] -> (Int, Int, Int)
parseColor v = case v of
    [FunctionValue ("rgb", comps)] -> case parseFuncValue comps of
        [PreservedValue (NumberToken r), PreservedValue (NumberToken g), PreservedValue (NumberToken b)] -> (round r, round g, round b)
    [FunctionValue ("hsl", comps)] -> case parseFuncValue comps of
        [PreservedValue (NumberToken h), PreservedValue (NumberToken s), PreservedValue (NumberToken l)] -> overColor round $ hslToRgb (h, s, l)
    [FunctionValue ("var", comps)] -> case parseFuncValue comps of
        (PreservedValue (DelimToken '-'):PreservedValue (DelimToken '-'):PreservedValue (IdentToken _):rest) -> case rest of
            [PreservedValue (HashToken _)] -> parseColor rest
            _ -> trace "ahhhhhhh" (0, 0, 0)
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
    _ -> trace ("Unknown color: " ++ show v) (0, 0, 0)
    where
        overColor f (r, g, b) = (f r, f g, f b)

        hslToRgb :: (Float, Float, Float) -> (Float, Float, Float)
        hslToRgb (_h, s, l) = ((r' + m) * 255, (b' + m) * 255, (g' + m) * 255)
            where
                h :: Float
                h = fromIntegral $ round _h `mod` (360 :: Integer)
                c = (1 - abs (2 * l - 1)) * s
                x = c * (1 - abs (fromIntegral $ (round (h / 60) `mod` (2 :: Integer)) - 1))
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
            "display" -> case vs of
                [PreservedValue (IdentToken "block")] -> set display Block
                [PreservedValue (IdentToken "inline")] -> set display Inline
                [PreservedValue (IdentToken "inline-block")] -> set display Inline
                [PreservedValue (IdentToken "inline-table")] -> set display Inline
                [PreservedValue (IdentToken "none")] -> set real False
                _ -> traceUnknown
            "position" -> case vs of
                [PreservedValue (IdentToken "relative")] -> set real True
                _ -> set real False
            "float" -> case vs of
                [PreservedValue (IdentToken "none")] -> set real True
                _ -> set real False
            "color" -> case vs of
                [PreservedValue (IdentToken "default")] -> set foregroundColor (0, 0, 0) 
                _ -> doColor $ set foregroundColor $ parseColor vs
            "background" -> doColor doBackground
            "background-color" -> doColor doBackground
            "font-weight" -> case vs of
                [PreservedValue (IdentToken "bold")] -> set bold True
                [PreservedValue (IdentToken "normal")] -> set bold False
                _ -> traceUnknown
            "font-style" -> case vs of
                [PreservedValue (IdentToken "italic")] -> set italicized True
                [PreservedValue (IdentToken "normal")] -> set italicized False
                _ -> traceUnknown
            "text-decoration" -> case vs of
                [PreservedValue (IdentToken "underline")] -> set underlined True
                [PreservedValue (IdentToken "line-through")] -> set struckthrough True
                [PreservedValue (IdentToken "none")] -> set struckthrough False . set underlined False
                _ -> traceUnknown
            _ -> id
            where
                doColor normal = if vs == [PreservedValue (IdentToken "inherit")]
                    then id
                    else normal

                doBackground = set backgroundColor $ case vs of
                    [PreservedValue (IdentToken "transparent")] -> Nothing
                    [PreservedValue (IdentToken "initial")] -> Just (255, 255, 255)
                    _ -> Just $ parseColor vs

                traceUnknown = trace ("Unknown " ++ n ++ ": " ++ show vs) id
        _ -> out

buildCSSTree :: [ComponentValue] -> CSSTree
buildCSSTree css = unfoldTree _buildCSSTree (blamCSS [] css)

blamCSS :: [SelectorNode] -> [ComponentValue] -> (PreSelectorNode, [SelectorNode])
blamCSS collapsed css = case css of
    [] -> (((CurrentCombinator, StarSelector), []), collapsed)
    (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock ss ds)) : rest) -> blamCSS (map (, ds) ss ++ collapsed) rest
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

countCSSTree :: Int -> CSSTree -> Int
countCSSTree num tree = case subForest tree of
    [] -> 1
    _ -> foldr ((+) . countCSSTree 0) num (subForest tree)

drawCSSTree :: [ComponentValue] -> String
drawCSSTree css = drawTree $ show . fst <$> buildCSSTree css

getAttr :: Tag -> String -> Maybe Attribute
getAttr t name = filter (\ (Attribute (n, _)) -> n == name) (_attrs t) L.!? 0

checkSelector :: Tag -> Selector -> Bool
checkSelector tag selector = case selector of
    StarSelector -> True
    (TagSelector n) -> n == _tagName tag
    (HashSelector n) -> n == getAttrJust tag "id"
    (ClassSelector n) ->
        let classes = splitOn " " $ getAttrJust tag "class"
        in elem n classes
    (AttrSelector a c) -> c (getAttrValue <$> getAttr tag a)
    (StateSelector _ _) -> False
    (NotSelector c) -> not $ checkSelector tag c
    (OrSelector c) -> any (checkSelector tag) c
    (AndSelector c) -> foldr (\ a b -> checkSelector tag a && b) True c

getAttrJust :: Tag -> String -> String
getAttrJust tag = maybe "" getAttrValue . getAttr tag
getAttrValue :: Attribute -> String
getAttrValue (Attribute (_, v)) = v

applyStyle :: [Tag] -> CSSTree -> CSSAttribute
applyStyle ts (Node _ c) = go (map (ts,) c)
    where
        go cs = case foldr doChildren (id, []) cs of
            (val, []) -> val
            (val, a) -> go a . val

        doChildren :: ([Tag], CSSTree) -> (CSSAttribute, [([Tag], CSSTree)]) -> (CSSAttribute, [([Tag], CSSTree)])
        doChildren (tag:tags, Node ((combinator, selector), value) children) (val, out) = case combinator of
            CurrentCombinator -> if checkSelector tag selector
                then toNext (tag:tags)
                else (val, out)
            ChildCombinator
                | null tags -> (val, out)
                | checkSelector (head tags) selector -> toNext tags
                | otherwise -> (val, out)
            DescendantCombinator -> case go' tags of
                (Just rest) -> toNext rest
                Nothing -> (val, out)
                where
                    go' [] = Nothing
                    go' (newTag:newTags) = if checkSelector newTag selector
                        then Just (newTag:newTags)
                        else go' newTags
            where
                toNext currentTags = (value . val, map (currentTags,) children ++ out)

