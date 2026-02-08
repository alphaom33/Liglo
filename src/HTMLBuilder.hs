{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HTMLBuilder where

import qualified Data.List as L
import qualified Data.Map as M
import Data.List.Split
import Data.Tree

import Debug.Trace

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over, ASetter)

import HTMLParser (Token(..), Tag, Attribute(..), _selfClosing, _opening, _tagName, tracer, _attrs)
import CSSParser
import CSSTokenizer
import qualified Mortar

import CharacterReferences
import Mortar (wrapWords, escapeLength)

type CSSAttribute = TextState -> TextState
instance Show CSSAttribute where
    show _ = "<attr>"

type PreSelectorNode = (SelectorData, [ComponentValue])
type SelectorNode = ([SelectorData], [ComponentValue])
type CSSTree = Tree (SelectorData, CSSAttribute)

data DisplayType =
    Inline
    | Block
    deriving (Show, Eq)

data TextState = TextState {
    _foregroundColor :: (Int, Int, Int)
    , _backgroundColor :: Maybe (Int, Int, Int)
    , _bold :: Bool
    , _italicized :: Bool
    , _underlined :: Bool
    , _struckthrough :: Bool
    , _real :: Bool
    , _display :: DisplayType
} deriving (Eq, Show)
$(makeLenses ''TextState)

data CheckedTextState =
    CheckedColor (Int, Int, Int)
    | CheckedMaybe (Maybe (Int, Int, Int))
    | CheckedBool Bool
    deriving (Show, Eq)

toChecked :: TextState -> [(Int, CheckedTextState)]
toChecked textState =
    let
        arrayed = [CheckedColor $ _foregroundColor textState, CheckedMaybe $ _backgroundColor textState] ++ map CheckedBool [_bold textState, _italicized textState, _underlined textState, _struckthrough textState, _real textState]
    in
        zip [0..] arrayed

data BuilderData = BuilderData {
    _toRead :: [Token]
    , _openTags :: [Tag]
    , _openStyles :: [TextState]
    , _currentStyle :: TextState
    , _out :: String
    , _style :: CSSTree
    , _lined :: Bool
    , _discard :: Bool
    , _size :: (Int, Int)
} deriving Show
$(makeLenses ''BuilderData)

data TableData = TableData {
    _tBuilderData :: BuilderData
    , _tOut :: [[String]]
    , _buffer :: String
    , _bufferer :: [String]
    , _posttion :: String
    , _caption :: String
} deriving Show
$(makeLenses ''TableData)

textState :: TextState
textState = TextState {_foregroundColor=(0, 0, 0), _backgroundColor=Just (255, 255, 255), _bold=False, _italicized=False, _underlined=False, _struckthrough=False, _real=True, _display=Block}

builderData :: [Token] -> CSSTree -> (Int, Int) -> BuilderData
builderData emittedTokens p_style p_size = BuilderData {_openTags=[], _openStyles=[textState], _currentStyle=textState, _toRead=emittedTokens, _out=[], _style=p_style, _lined=True, _discard=False, _size=p_size}

tableData :: BuilderData -> TableData
tableData p_builderData = TableData {_tBuilderData=p_builderData, _tOut=[], _buffer="", _bufferer=[], _posttion="", _caption=""}

makeWidget :: BuilderData -> TextState
makeWidget mhm = applyStyle (_openTags mhm) (_style mhm) (head $ _openStyles mhm)

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
            DescendantCombinator -> case go tags of
                (Just rest) -> toNext rest
                Nothing -> (val, out)
                where
                    go [] = Nothing
                    go (tag:tags) = if checkSelector tag selector
                        then Just (tag:tags)
                        else go tags
            where
                toNext tags = (value . val, map (tags,) children ++ out)

parseTag :: Tag -> BuilderData -> BuilderData
parseTag t mhm
        | _tagName t == "br" && not (_discard mhm) = appendHbox mhm
        | _opening t =
            (if _tagName t == "table" then runTable else id)
            . addStyle
            . over openTags (t:)
            $ mhm
        | not $ _opening t = killExtras mhm
        where
            runTable mhm' =
                let
                    newData = buildTable buffer $ tableData $ set lined True $ set out "" $ over toRead (drop 1) mhm'
                    tableString = outputTable newData
                in
                    over out (((_posttion newData ++ tableString ++ (if null $ _caption newData then "" else '\n':_caption newData)) ++) . (if not (null $ _out mhm') && head (_out mhm') == '\n' then id else ('\n':)))
                    . applyStateDiff --TODO does this actually do anything?
                    . set lined True
                    . set out (_out mhm')
                    $ _tBuilderData newData

            killExtras mhm'
                | null $ _openTags mhm' = mhm'
                | (_tagName . head . _openTags $ mhm) /= _tagName t =
                    killExtras
                    . doEnd
                    $ mhm'
                | otherwise = doEnd mhm'

            doEnd mhm' =
                over openTags (drop 1)
                . endings (head $ _openTags mhm')
                $ mhm'

            addStyle state = over openStyles (makeWidget state:) state

            endings tag state =
                (if _display (_currentStyle state) == Block && _tagName tag `elem` ["li", "h1", "h2", "h3", "h4", "h5", "h6", "p", "pre", "div", "dt", "dd"] && not (_discard state)
                    then appendHbox
                    else id)
                . applyStateDiff
                . over openStyles (drop 1)
                $ state

parseCharacter :: Char -> BuilderData -> BuilderData
parseCharacter c mhm
    | _discard mhm = mhm
    | c `elem` " \n\t" && "pre" `notElem` map _tagName (_openTags mhm) = killWhitespace mhm
    | not (null (_openTags mhm)) && null (["head", "meta", "link", "script", "style", "select"] `L.intersect` map _tagName (_openTags mhm)) =
        over out (c:)
        . set lined False
        $ mhm
    | otherwise = mhm

_buildHtml :: BuilderData -> BuilderData

_buildHtml mhm = case _toRead mhm of
    (TagToken t:_) -> _buildHtml $ parseTag t mhm'
    (Character c:_) ->
        _buildHtml
        . parseCharacter c
        . applyStateDiff
        $ mhm'

    (_:_) -> _buildHtml mhm'

    [] -> mhm
    where
        mhm' = over toRead (drop 1) mhm

buildTable :: ASetter TableData TableData String String -> TableData -> TableData
buildTable lens m = case head $ _toRead bData of
    (TagToken t) -> case (_opening t, _tagName t) of
        (False, "table") -> set posttion (_buffer mhm) mhm
        (True, "caption") -> buildTable caption $ doATag caption t mhm
        (False, "caption") -> buildTable buffer $ over tBuilderData (set lined True) $ doATag caption t mhm

        x -> buildTable lens $ doATag lens t $ case x of

            (False, "tr") ->
                over tBuilderData (set lined True)
                . set bufferer []
                . over tOut (_bufferer mhm:)
                . elemateIt
                $ mhm

            (False, "td") -> closeTd
            (False, "th") -> closeTd

            _ -> mhm

    (Character c) -> buildTable lens $ doACharacter lens c mhm
    where

        doACharacter lens c mhm =
            let bData' = parseCharacter c . applyStateDiff . set out "" $ bData
            in over lens (_out bData'++) $ over tBuilderData (set lined (_lined bData') . set currentStyle (_currentStyle bData') . over toRead (drop 1)) mhm

        doATag :: ASetter TableData TableData [Char] [Char] -> Tag -> TableData -> TableData
        doATag lens t = (\ umhum -> over lens (_out (_tBuilderData umhum)++) umhum) . over tBuilderData (over toRead (drop 1) . parseTag t)

        closeTd = elemateIt . over tBuilderData (set lined True) $ mhm

        bData = _tBuilderData mhm

        elemateIt = set buffer "" . over bufferer (_buffer mhm:)

        mhm = over tBuilderData (set out "") m

outputTable :: TableData -> String
outputTable mhm = tableIt $ bottomed vHeight broken
    where
        rowIt [e] = space e ++ e
        rowIt (e:es) = space e ++ e ++ rowIt es
        rowIt [] = ""

        tableIt [r] = rowIt r
        tableIt (r:rs) = rowIt r ++ "\n" ++ tableIt rs
        tableIt [] = ""

        bData = _tBuilderData mhm
        hWidth = fst (_size bData) `div` length (head $ _tOut mhm)
        space e = replicate (hWidth - escapeLength (reverse e)) ' '

        broken :: [[[String]]]
        broken = map (map (map reverse . wrapWords hWidth . reverse)) $ _tOut mhm
        vHeight = map (maximum . map length) broken

        bottomed :: [Int] -> [[[String]]] -> [[String]]
        bottomed (h:hs) (r:rs) = go h ++ bottomed hs rs
            where 
                go :: Int -> [[String]]
                go (-1) = []
                go i = map (\ x -> case x L.!? i of
                    (Just x) -> x
                    Nothing -> []) r : go (i - 1)
        bottomed [] [] = []

applyStateDiff :: BuilderData -> BuilderData
applyStateDiff state = foldr (\ (old, new) b -> if old == new then b else applyStateElement new b) state' $ zip (toChecked oldStyle) (toChecked newStyle)
    where
        oldStyle = _currentStyle state
        newStyle = head $ _openStyles state
        state' = set currentStyle newStyle state

applyStateElement :: (Eq a, Num a) => (a, CheckedTextState) -> BuilderData -> BuilderData
applyStateElement new = case new of
    (0, CheckedColor (r, g, b)) -> undiscardedPutText $ Mortar.setForegroundColor r g b

    (1, CheckedMaybe Nothing) -> undiscardedPutText Mortar.resetBackground
    (1, CheckedMaybe (Just (r, g, b))) -> undiscardedPutText $ Mortar.setBackgroundColor r g b

    (2, CheckedBool True) -> undiscardedPutText Mortar.bold
    (2, CheckedBool False) -> undiscardedPutText Mortar.resetBold

    (3, CheckedBool True) -> undiscardedPutText Mortar.italic
    (3, CheckedBool False) -> undiscardedPutText Mortar.resetItalic

    (4, CheckedBool True) -> undiscardedPutText Mortar.underline
    (4, CheckedBool False) -> undiscardedPutText Mortar.resetUnderline

    (5, CheckedBool True) -> undiscardedPutText Mortar.strikethrough
    (5, CheckedBool False) -> undiscardedPutText Mortar.resetStrikethrough

    (6, CheckedBool b) -> set discard (not b)
    where
        undiscardedPutText out state = if _real $ _currentStyle state
            then putText out state
            else state


putText :: [Char] -> BuilderData -> BuilderData
putText text = over out (reverse text++)

appendHbox :: BuilderData -> BuilderData
appendHbox = set lined True . putText "\n"

killWhitespace :: BuilderData -> BuilderData
killWhitespace mhm = _killWhitespace $ (if _lined mhm
    then id
    else over out (' ':)) $ set lined True mhm

_killWhitespace :: BuilderData -> BuilderData
_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then _killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (head $ _toRead mhm, over toRead (drop 1) mhm)

parseFuncValue :: [ComponentValue] -> [ComponentValue]
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
                _ -> id
            "position" -> case vs of
                [PreservedValue (IdentToken "relative")] -> set real True
                _ -> set real False
            "float" -> case vs of
                [PreservedValue (IdentToken "none")] -> set real True
                _ -> set real False
            "color" -> set foregroundColor $ parseColor vs
            "background" -> set backgroundColor $ if vs == [PreservedValue (IdentToken "transparent")]
                then Nothing
                else Just $ parseColor vs
            "background-color" -> set backgroundColor $ Just $ parseColor vs
            "font-weight" -> case vs of
                [PreservedValue (IdentToken "bold")] -> set bold True
                [PreservedValue (IdentToken "normal")] -> set bold False
            "font-style" -> case vs of
                [PreservedValue (IdentToken "italic")] -> set italicized True
                [PreservedValue (IdentToken "normal")] -> set italicized False
            "text-decoration" -> case vs of
                [PreservedValue (IdentToken "underline")] -> set underlined True
                [PreservedValue (IdentToken "line-through")] -> set struckthrough True
                [PreservedValue (IdentToken "none")] -> set struckthrough False . set underlined False
            _ -> id
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

parseWebpage :: [Token] -> [ComponentValue] -> (Int, Int) -> String
parseWebpage emittedTokens css = reverse . _out . _buildHtml . builderData emittedTokens (buildCSSTree css)

parseTableTest :: [Token] -> [ComponentValue] -> (Int, Int) -> String
parseTableTest emittedTokens css eyes = outputTable $ buildTable buffer (TableData {_tBuilderData=builderData emittedTokens (buildCSSTree css) eyes, _tOut=[], _buffer="", _bufferer=[], _posttion="", _caption=""})

drawCSSTree :: [ComponentValue] -> String
drawCSSTree css = drawTree $ show . fst <$> buildCSSTree css
