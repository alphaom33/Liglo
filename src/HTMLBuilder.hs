{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module HTMLBuilder where

import qualified Data.List as L
import Data.List.Split

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over, ASetter)

import HTMLParser (Token(..), Tag, _opening, _tagName)
import CSSParser (ComponentValue)
import CSSBuilder
import qualified Mortar
import TextState

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
    , _webString :: String
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
builderData emittedTokens p_style p_size = BuilderData {_openTags=[], _openStyles=[textState], _currentStyle=textState, _toRead=emittedTokens, _webString=[], _style=p_style, _lined=True, _discard=False, _size=p_size}

tableData :: BuilderData -> TableData
tableData p_builderData = TableData {_tBuilderData=p_builderData, _tOut=[], _buffer="", _bufferer=[], _posttion="", _caption=""}

makeWidget :: BuilderData -> TextState
makeWidget mhm = applyStyle (_openTags mhm) (_style mhm) (head $ _openStyles mhm)

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
                    newData = buildTable buffer $ tableData $ set lined True $ set webString "" $ over toRead (drop 1) mhm'
                    tableString = outputTable newData
                in
                    over webString (((_posttion newData ++ tableString ++
                        (if null $ _caption newData then "" else '\n':_caption newData)) ++)
                        . ('\n':)
                        . (if not (null $ _webString mhm') && head (_webString mhm') == '\n' then id else ('\n':)))
                    . applyStateDiff --TODO does this actually do anything?
                    . set lined True
                    . set webString (_webString mhm')
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
        over webString (c:)
        . set lined False
        $ mhm
    | otherwise = mhm

buildHtml :: BuilderData -> BuilderData
buildHtml mhm = case _toRead mhm of
    (TagToken t:_) -> buildHtml $ parseTag t mhm'
    (Character c:_) ->
        buildHtml
        . parseCharacter c
        . applyStateDiff
        $ mhm'

    (_:_) -> buildHtml mhm'

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

        doACharacter charLens c =
            let bData' = parseCharacter c . applyStateDiff . set webString "" $ bData
            in over charLens (_webString bData'++) . over tBuilderData (set lined (_lined bData') . set currentStyle (_currentStyle bData') . over toRead (drop 1))

        doATag :: ASetter TableData TableData [Char] [Char] -> Tag -> TableData -> TableData
        doATag tagLens t = (\ umhum -> over tagLens (_webString (_tBuilderData umhum)++) umhum) . over tBuilderData (over toRead (drop 1) . parseTag t)

        closeTd = elemateIt . over tBuilderData (set lined True) $ mhm

        bData = _tBuilderData mhm

        elemateIt = set buffer "" . over bufferer (_buffer mhm:)

        mhm = over tBuilderData (set webString "") m

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
        space e = replicate (hWidth - Mortar.escapeLength (reverse e)) ' '

        broken :: [[[String]]]
        broken = map (map (foldr ((\ x a -> splitOn "\n" x ++ a) . reverse) [] . Mortar.wrapWords hWidth . reverse . dropWhile (== '\n'))) $ _tOut mhm
        vHeight = map (maximum . map length) broken

        bottomed :: [Int] -> [[[String]]] -> [[String]]
        bottomed (h:hs) (r:rs) = go h ++ bottomed hs rs
            where
                go :: Int -> [[String]]
                go (-1) = []
                go i = map (\ x -> case x L.!? i of
                    (Just x') -> x'
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
putText text = over webString (reverse text++)

appendHbox :: BuilderData -> BuilderData
appendHbox = set lined True . putText "\n"

killWhitespace :: BuilderData -> BuilderData
killWhitespace mhm = _killWhitespace $ (if _lined mhm
    then id
    else over webString (' ':)) $ set lined True mhm

_killWhitespace :: BuilderData -> BuilderData
_killWhitespace mhm = case next of
    (Character c) -> if c `elem` " \n\t"
        then _killWhitespace mhm'
        else mhm
    _ -> mhm
    where (next, mhm') = (head $ _toRead mhm, over toRead (drop 1) mhm)

parseWebpage :: [Token] -> [ComponentValue] -> (Int, Int) -> String
parseWebpage emittedTokens css = reverse . _webString . buildHtml . builderData emittedTokens (buildCSSTree css)
