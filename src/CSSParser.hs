{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CSSParser where

import CSSTokenizer

import Data.List.Split

import Data.Map ((!), fromList)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.List (intersect)
import Debug.Trace (trace)
import HTMLParser (tracer)

type AttrNess = (Maybe String -> Bool)
instance Show AttrNess where
    show _ = "<attr>"
instance Eq AttrNess where
    (==) _ _ = True

type SelectorData = (Combinator, Selector)

data Combinator =
    DescendantCombinator
    | ChildCombinator
    | CurrentCombinator
    deriving (Show, Eq)

data Selector =
    TagSelector String
    | HashSelector String
    | ClassSelector String
    | StateSelector String String
    | StarSelector
    | AttrSelector String AttrNess
    | NotSelector Selector
    | OrSelector [Selector]
    | AndSelector [Selector]
    deriving (Show, Eq)

data CurlyBlock = CurlyBlock [[SelectorData]] [ComponentValue] deriving (Show, Eq)
newtype SquareBlock = SquareBlock [ComponentValue] deriving (Show, Eq)
newtype ParenthesisBlock = ParenthesisBlock [ComponentValue] deriving (Show, Eq)

data SimpleBlock =
    SimpleCurlyBlock CurlyBlock
    | SimpleSquareBlock SquareBlock
    | SimpleParenthesisBlock ParenthesisBlock
    deriving (Show, Eq)

data Declaration = Declaration {
        declarationName :: String
        , declarationValue :: [ComponentValue]
        , declarationImportant :: Bool
    } deriving (Show, Eq)

data ComponentValue =
    PreservedValue CSSToken
    | FunctionValue (String, [ComponentValue])
    | SimpleBlockValue SimpleBlock
    | DeclarationValue Declaration
    | RuleValue Rule
    | SelectorValue Selector
    deriving (Show, Eq)

data Rule =
    AtRule {
        atName :: String
        , atPrelude :: [ComponentValue]
        , atBlock :: Maybe CurlyBlock
    }
    deriving (Show, Eq)

parseList :: [CSSToken] -> [ComponentValue]
parseList list = reverse $ _parseList [] list

consumeFunction :: [CSSToken] -> (ComponentValue, [CSSToken])
consumeFunction ((FunctionToken name):s) = go [] s
    where
        go :: [ComponentValue] -> [CSSToken] -> (ComponentValue, [CSSToken])
        go out state = if nextInputToken `elem` [ClosingParenthesisToken, EOFToken]
            then (FunctionValue (name, reverse out), state')
            else let (val, state'') = consumeFuncValue state in go (val:out) state''
            where
                (nextInputToken:state') = state

        consumeFuncValue (nextInputToken:state) = case nextInputToken of
            (FunctionToken f) -> consumeFunction (FunctionToken f : state)
            _ -> (PreservedValue nextInputToken, state)

consumeComponentValue :: [CSSToken] -> (ComponentValue, [CSSToken])
consumeComponentValue = go []
    where
        go tokens state = case nextInputToken of
            OpeningCurlyBracketToken -> consumeSimpleBlock (reverse tokens) state
            -- OpeningSquareBracketToken -> consumeSimpleBlock (reverse tokens) state
            OpeningParenthesisToken -> consumeSimpleBlock (reverse tokens) state
            (FunctionToken _) -> consumeFunction state
            (AtKeywordToken _) -> consumeAtRule state
            _ -> if nextnextInputToken == EOFToken
                then (PreservedValue nextInputToken, state')
                else go (PreservedValue nextInputToken : tokens) state'
            where
                (nextInputToken:state') = state
                (nextnextInputToken:_) = state'

killrest :: [CSSToken] -> [CSSToken]
killrest = go 1
    where
        go :: Integer -> [CSSToken] -> [CSSToken]
        go count (next:state)
            | next == OpeningCurlyBracketToken = go (count + 1) state
            | next == ClosingCurlyBracketToken = if count == 0
                then state
                else go (count - 1) state
            | otherwise = go count state

matchspaced :: [Char] -> [Char] -> Bool
matchspaced s = elem s . splitOn " "

eatSquare :: [CSSToken] -> ([CSSToken], [CSSToken])
eatSquare (ClosingSquareBracketToken : rest) = (rest, [ClosingSquareBracketToken])
eatSquare (WhitespaceToken : rest) = eatSquare rest
eatSquare (next : rest) = (next:) <$> eatSquare rest

ackackack :: [SelectorData] -> [SelectorData] -> [SelectorData] -> [SelectorData] -- TODO fix this absolutely idiotic nonsense
ackackack mhm out [] = if null mhm
    then out
    else (CurrentCombinator, AndSelector (map snd mhm)):out

ackackack mhm out ((c, s):ss) = case c of
    CurrentCombinator -> ackackack ((c, s) : mhm) out ss
    _ -> if null mhm
        then ackackack [] ((c,s):out) ss
        else ackackack [] ((c, AndSelector (s:map snd mhm)):out) ss

parseTokenList :: [[SelectorData]] -> [SelectorData] -> [ComponentValue] -> [[SelectorData]]
parseTokenList _out _currentList _tokens = map (ackackack [] [] . reverse) $ go _out _currentList $ dropWhile (== WhitespaceToken) $ map (\ (PreservedValue p) -> p) _tokens
    where
        go :: [[SelectorData]] -> [SelectorData] -> [CSSToken] -> [[SelectorData]]
        go out [] [] = reverse out
        go out currentList [] = go (currentList : out) [] []

        go out currentList (ColonToken : IdentToken pseudoClass : tokens) = if pseudoClass == "link"
            then go out currentList tokens
            else go out [] (drop 1 $ dropWhile (/= CommaToken) tokens)
        go out currentList (ColonToken : ColonToken : IdentToken _ : tokens) = go out currentList tokens

        go out currentList [WhitespaceToken] = go out currentList []

        go out currentList (next : tokens) = case next of
            CommaToken -> go addCurrentList [] tokens
            _ -> onward (next:tokens)
            where
                onward css =
                    let
                        (selector, rest) = matchSelector css
                        (combinator, rest') = matchCombinator rest
                    in
                        go out ((combinator, selector) : currentList) rest'
                addCurrentList = currentList : out

        getFunction tokens = (yep, rest)
            where
                arguments = takeWhile (/= ClosingParenthesisToken) tokens
                argumentList = filter (not . (`elem` [WhitespaceToken, CommaToken])) arguments
                yep = map tokenToSelector argumentList
                (_:rest) = dropWhile (/= ClosingParenthesisToken) tokens

        matchSelector :: [CSSToken] -> (Selector, [CSSToken])
        matchSelector css = case css of
            (WhitespaceToken : rest) -> matchSelector rest

            (DelimToken '*' : rest) -> (StarSelector, rest)

            (ColonToken : FunctionToken "where" : tokens) ->
                let (yep, rest) = getFunction tokens
                in (OrSelector yep, rest)

            -- TODO is should add specificity
            (ColonToken : FunctionToken "is" : tokens) ->
                let (yep, rest) = getFunction tokens
                in (OrSelector yep, rest)

            (ColonToken : FunctionToken "not" : tokens) ->
                let (yep, rest) = getFunction tokens
                in (NotSelector $ OrSelector yep, rest)

            -- TODO um no
            (ColonToken : FunctionToken _ : tokens) ->
                let (_, rest) = getFunction tokens
                in (NotSelector StarSelector, rest)

            (OpeningSquareBracketToken : IdentToken attr : rest) -> case square of
                (DelimToken '=' : rest'') -> getAttrSelector (==) rest''
                (DelimToken '~' : DelimToken '=' : rest'') -> getAttrSelector matchspaced rest''
                (DelimToken '|' : DelimToken '=' : rest'') -> getAttrSelector (\ against v -> v == against || take (length against + 1) v == against ++ "-") rest''
                (DelimToken '^' : DelimToken '=' : rest'') -> getAttrSelector (\ against -> (== against) . take (length against)) rest''
                (DelimToken '$' : DelimToken '=' : rest'') -> getAttrSelector (\ against -> (== against) . reverse . take (length against) . reverse) rest''
                (DelimToken '*' : DelimToken '=' : rest'') -> getAttrSelector matchStar rest''
                rest'' -> addAttrSelector (/= Nothing) rest''
                where
                    (rest', square) = eatSquare rest

                    getAttrSelector check (IdentToken against : rest'') = addAttrSelector (check against . fromMaybe "") rest''
                    getAttrSelector check (StringToken against : rest'') = addAttrSelector (check against . fromMaybe "") rest''

                    addAttrSelector check rest'' = case rest'' of
                        (IdentToken "i" : _) -> finish (map toLower attr) (check . fmap (map toLower))
                        (IdentToken "I" : _) -> finish (map toLower attr) (check . fmap (map toLower))
                        (IdentToken "s" : _) -> finish attr check
                        (IdentToken "S" : _) -> finish attr check
                        _ -> finish attr check
                        where
                            finish attr' check' = (AttrSelector attr' check', rest')


            (nextToken : rest) -> (tokenToSelector nextToken, rest)

        tokenToSelector nextToken = case nextToken of
            (IdentToken n) -> TagSelector n
            (ClassToken n) -> ClassSelector n
            (HashToken (_, n)) -> HashSelector n
            _ -> trace ("Unkown token: " ++ show nextToken) StarSelector

matchStar :: Eq a => [a] -> [a] -> Bool
matchStar tokens against = go against (length against - length tokens)
    where
        go v num
            | num < 0 = False
            | tokens == take (length tokens) v = True
            | otherwise = go (drop 1 v) (num - 1)


matchCombinator :: [CSSToken] -> (Combinator, [CSSToken])
matchCombinator [] = (CurrentCombinator, [])
matchCombinator tokens = (getCombinator, rest)
    where
        getCombinator
            | not $ null $ [EOFToken, CommaToken] `intersect` eatWhitten = CurrentCombinator
            | DelimToken '>' `elem` eatWhitten = ChildCombinator
            | DelimToken '~' `elem` eatWhitten = ChildCombinator -- TODO SubsequentSiblingCombinator
            | DelimToken '+' `elem` eatWhitten = ChildCombinator -- TODO SiblingCombinator
            | WhitespaceToken `elem` eatWhitten = DescendantCombinator
            | otherwise = CurrentCombinator

        (rest, eatWhitten) = eatWhite tokens

        eatWhite :: [CSSToken] -> ([CSSToken], [CSSToken])
        eatWhite [] = ([], [EOFToken])
        eatWhite (WhitespaceToken : rest') = (WhitespaceToken :) <$> eatWhite rest'
        eatWhite (DelimToken '>' : rest') = (rest', [DelimToken '>'])
        eatWhite (DelimToken '~' : rest') = (rest', [DelimToken '~'])
        eatWhite (DelimToken '+' : rest') = (rest', [DelimToken '+'])
        eatWhite (next : rest') = (next:rest', [next])

consumeSimpleBlock :: [ComponentValue] -> [CSSToken] -> (ComponentValue, [CSSToken])
consumeSimpleBlock tokens (startingToken:s) =
    let (val, state') = go [] s
    in (SimpleBlockValue $ (startToConstructor ! startingToken) $ reverse val, state')
    where
        go :: [CSSToken] -> [CSSToken] -> ([ComponentValue], [CSSToken])
        go out state
            | nextInputToken `elem` [endingToken, EOFToken] = (fst $ consumeListOfDeclarations (reverse $ EOFToken : out), state')
            | nextInputToken == startingToken && startingToken == OpeningCurlyBracketToken = ([], killrest state')
            | otherwise = go (nextInputToken:out) state'
            where (nextInputToken:state') = state

        endingToken = startToEnd ! startingToken

        startToEnd = fromList [
            (OpeningCurlyBracketToken, ClosingCurlyBracketToken)
            , (OpeningSquareBracketToken, ClosingSquareBracketToken)
            , (OpeningParenthesisToken, ClosingParenthesisToken)
            ]

        startToConstructor = fromList [
            (OpeningCurlyBracketToken, SimpleCurlyBlock . CurlyBlock (parseTokenList [] [] tokens))
            , (OpeningSquareBracketToken, SimpleSquareBlock . SquareBlock)
            , (OpeningParenthesisToken, SimpleParenthesisBlock . ParenthesisBlock)
            ]

consumeAtRule :: [CSSToken] -> (ComponentValue, [CSSToken])
consumeAtRule ((AtKeywordToken name):s) = go [] s
    where
        go prelude state = case nextInputToken of
            SemicolonToken -> (attish Nothing, state')
            EOFToken -> (attish Nothing, state)
            OpeningCurlyBracketToken -> let (SimpleBlockValue (SimpleCurlyBlock val), state'') = consumeSimpleBlock [] state in (attish (Just val), state'')
            _ -> go prelude state'
            where
                attish block = RuleValue AtRule {atName=name, atPrelude=prelude, atBlock=block}
                (nextInputToken:state') = state

consumeDeclaration :: [CSSToken] -> (Maybe Declaration, [CSSToken])
consumeDeclaration s = if nextInputToken == ColonToken
    then
        let
            (val, state'') = eatVals [] $ eatWhitespace state'
        in case val of
            ((PreservedValue (IdentToken "important")):(PreservedValue (DelimToken '!')):rest) -> (Just $ declar rest True, state'')
            _ -> (Just $ declar val False, state'')
    else (Nothing, nextInputToken:state')
    where
        ((IdentToken name):state) = s
        (nextInputToken:state') = state

        eatWhitespace whitespaceState = if whiteSpaceNextInputToken == WhitespaceToken
            then eatWhitespace whitespaceState' 
            else whitespaceState
            where (whiteSpaceNextInputToken:whitespaceState') = whitespaceState

        eatVals :: [ComponentValue] -> [CSSToken] -> ([ComponentValue], [CSSToken])
        eatVals vals valsState = case valsNextInputToken of
            EOFToken -> (vals, valsState)
            WhitespaceToken -> eatVals vals valsState'
            _ -> let (val, valsState'') = consumeComponentValue valsState in eatVals (val:vals) valsState''
            where (valsNextInputToken:valsState') = valsState

        declar val imp = Declaration {declarationName=name, declarationValue=reverse val, declarationImportant=imp}

killUnknown :: [CSSToken] -> [CSSToken]
killUnknown state = if nextInputToken `elem` [SemicolonToken, EOFToken]
    then killUnknown $ snd $ consumeComponentValue state
    else state'
    where
        (nextInputToken:state') = state

consumeIdent :: [CSSToken] -> (Maybe Declaration, [CSSToken])
consumeIdent = go []
    where
        go :: [CSSToken] -> [CSSToken] -> (Maybe Declaration, [CSSToken])
        go tokens state = if nextInputToken `elem` [SemicolonToken, EOFToken]
            then let (val, _) = consumeDeclaration $ reverse (EOFToken:tokens) in case val of
                (Just val') -> (Just val', state)
                Nothing -> (Nothing, state')
            else go (nextInputToken:tokens) state'
            where (nextInputToken:state') = state

consumeListOfDeclarations :: [CSSToken] -> ([ComponentValue], [CSSToken])
consumeListOfDeclarations = go []
    where
        go :: [ComponentValue] -> [CSSToken] -> ([ComponentValue], [CSSToken])
        go list [] = (list, [EOFToken])
        go list state = case nextInputToken of
            WhitespaceToken -> go list state'
            SemicolonToken -> go list state'
            EOFToken -> (list, state)
            (AtKeywordToken _) -> let (val, state'') = consumeAtRule state in go (val:list) state''
            (IdentToken _) -> let (val, state'') = consumeIdent state in case val of
                (Just val') -> go (DeclarationValue val':list) state''
                Nothing -> go list state''
            _ -> go list $ killUnknown state
            where
                (nextInputToken:state') = state

_parseList :: [ComponentValue] -> [CSSToken] -> [ComponentValue]
_parseList out state = if nextInputToken == EOFToken
    then reverse out
    else let (val, state') = consumeComponentValue state in _parseList (val:out) state'
    where nextInputToken = head state

outList :: String -> String -> [ComponentValue] -> String
outList out _ [] = out
outList out delim (val:rest) = outList (out ++ (case val of
    (PreservedValue ColonToken) -> ":"
    (PreservedValue SemicolonToken) -> ";"
    (PreservedValue CommaToken) -> ","
    (PreservedValue (IdentToken n)) -> n
    (PreservedValue OpeningCurlyBracketToken) -> "{"
    (PreservedValue ClosingCurlyBracketToken) -> "}"
    (PreservedValue OpeningSquareBracketToken) -> "["
    (PreservedValue ClosingSquareBracketToken) -> "]"
    (PreservedValue OpeningParenthesisToken) -> "("
    (PreservedValue ClosingParenthesisToken) -> ")"
    (PreservedValue (DelimToken c)) -> [c]
    (PreservedValue (NumberToken n)) -> show n
    (PreservedValue (DimensionToken (n, d))) -> show n ++ d
    (PreservedValue (PercentageToken n)) -> show n ++ "%"
    (PreservedValue (HashToken (_, n))) -> "#" ++ n
    (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock t c))) -> foldr (\ ss -> (outList "" "" (map (SelectorValue . snd) ss) ++) . (", " ++)) "" t ++ " {\n" ++ replaceAll "\n" "\n\t" (outList "\t" "" c) ++ "}\n\n"
    (SimpleBlockValue (SimpleSquareBlock (SquareBlock c))) -> "[" ++ outList "" "" c ++ "]"
    (SimpleBlockValue (SimpleParenthesisBlock (ParenthesisBlock c))) -> "(" ++ outList "" "" c ++ ")"
    (DeclarationValue (Declaration n v i)) -> n ++ ": " ++ outList "" " " v ++ (if i then "!important" else "") ++ ";\n"
    (SelectorValue (TagSelector s)) -> ' ' : s
    (SelectorValue (HashSelector s)) -> ' ' : s
    (SelectorValue (ClassSelector s)) -> ' ' : s
    (SelectorValue (StateSelector s l)) -> ' ' : s ++ ':' : l
    _ -> show val
    ) ++ delim) delim rest

