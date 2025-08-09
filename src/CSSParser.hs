{-# LANGUAGE TemplateHaskell #-}

module CSSParser where

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)

import CSSTokenizer
import HTMLParser (tracer)

import Data.Map ((!), fromList)

data Selector =
    TagSelector String
    | HashSelector String
    | ClassSelector String
    | StateSelector String String
    | StarSelector
    deriving (Show, Eq)

data CurlyBlock = CurlyBlock [[Selector]] [ComponentValue] deriving (Show, Eq)
data SquareBlock = SquareBlock [ComponentValue] deriving (Show, Eq)
data ParenthesisBlock = ParenthesisBlock [ComponentValue] deriving (Show, Eq)

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
    | QualifiedRule {
        qualifiedPrelude :: [ComponentValue]
        , qualifiedBlock :: CurlyBlock
    }
    | DeclarationRule Declaration
    deriving (Show, Eq)

parseList list = reverse $ _parseList [] list

consumeFunction s = go [] state
    where
        go :: [ComponentValue] -> [CSSToken] -> (ComponentValue, [CSSToken])
        go out state = if nextInputToken `elem` [ClosingParenthesisToken, EOFToken]
            then (FunctionValue (name, out), state)
            else let (val, state'') = consumeComponentValue state in go (val:out) state''
            where
                (nextInputToken:state') = state
        ((FunctionToken name):state) = s

consumeComponentValue :: [CSSToken] -> (ComponentValue, [CSSToken])
consumeComponentValue state = go [] state
    where
        go tokens state = case nextInputToken of
            OpeningCurlyBracketToken -> consumeSimpleBlock (reverse tokens) state
            OpeningSquareBracketToken -> consumeSimpleBlock (reverse tokens) state
            OpeningParenthesisToken -> consumeSimpleBlock (reverse tokens) state
            (FunctionToken _) -> consumeFunction state
            _ -> if nextnextInputToken == EOFToken
                then (PreservedValue nextInputToken, state')
                else go (PreservedValue nextInputToken : tokens) state'
            where
                (nextInputToken:state') = state
                (nextnextInputToken:_) = state'

consumeSimpleBlock :: [ComponentValue] -> [CSSToken] -> (ComponentValue, [CSSToken])
consumeSimpleBlock tokens s = 
    let (val, state') = go [] state
    in (SimpleBlockValue $ (startToConstructor ! startingToken) $ reverse val, state')
    where
        go :: [CSSToken] -> [CSSToken] -> ([ComponentValue], [CSSToken])
        go out state = if nextInputToken `elem` [endingToken, EOFToken]
            then (fst $ consumeListOfDeclarations (reverse $ EOFToken : out), state')
            else go (nextInputToken:out) state'
            where (nextInputToken:state') = state

        endingToken = startToEnd ! startingToken 

        startToEnd = fromList [
            (OpeningCurlyBracketToken, ClosingCurlyBracketToken)
            , (OpeningSquareBracketToken, ClosingSquareBracketToken)
            , (OpeningParenthesisToken, ClosingParenthesisToken)
            ]

        parseTokenList :: [[Selector]] -> [Selector] -> [ComponentValue] -> [[Selector]]
        parseTokenList out currentList tokens = case tokens of
            [] -> reverse $ if null currentList
                then out
                else addCurrentList
            ((PreservedValue (DelimToken '*')) : rest) -> parseTokenList out (StarSelector : currentList) rest
            ((PreservedValue locator) : (PreservedValue ColonToken) : (PreservedValue pseudoClass) : rest) -> 
                parseTokenList out ((StateSelector (grabStr locator) (grabStr pseudoClass)) : currentList) rest
                where 
                    grabStr token = case token of
                        (IdentToken s) -> s
                        (HashToken (_, s)) -> s
            ((PreservedValue nextToken) : rest) -> case nextToken of
                (IdentToken n) -> parseTokenList out ((case n of
                    ('.':n) -> ClassSelector n
                    _ -> TagSelector n) : currentList) rest
                (HashToken (_, n)) -> parseTokenList out (HashSelector n : currentList) rest
                CommaToken -> parseTokenList addCurrentList [] rest
            where
                addCurrentList = reverse currentList : out

        startToConstructor = fromList [
            (OpeningCurlyBracketToken, SimpleCurlyBlock . (CurlyBlock $ parseTokenList [] [] tokens))
            , (OpeningSquareBracketToken, SimpleSquareBlock . SquareBlock)
            , (OpeningParenthesisToken, SimpleParenthesisBlock . ParenthesisBlock)
            ]

        (startingToken:state) = s

consumeQualifiedRule :: [CSSToken] -> (Maybe Rule, [CSSToken])
consumeQualifiedRule state = go [] state
    where 
        go :: [ComponentValue] -> [CSSToken] -> (Maybe Rule, [CSSToken])
        go prelude state = case nextInputToken of
            EOFToken -> (Nothing, state)
            OpeningCurlyBracketToken -> 
                let ((SimpleBlockValue (SimpleCurlyBlock val)), state'') = consumeSimpleBlock [] state
                in (Just QualifiedRule {qualifiedPrelude=prelude, qualifiedBlock=val}, state'')
            _ -> let (val, state'') = consumeComponentValue state in go (val:prelude) state''
            where
                (nextInputToken:state') = state

consumeAtRule :: [CSSToken] -> (Rule, [CSSToken])
consumeAtRule s = go [] state
    where 
        go :: [ComponentValue] -> [CSSToken] -> (Rule, [CSSToken])
        go prelude state = case nextInputToken of
            SemicolonToken -> (attish Nothing, state')
            EOFToken -> (attish Nothing, state)
            OpeningCurlyBracketToken -> let ((SimpleBlockValue (SimpleCurlyBlock val)), state'') = consumeSimpleBlock [] state in (attish (Just val), state'')
            _ -> let (val, state'') = consumeComponentValue state in go (val:prelude) state''
            where
                attish block = AtRule {atName=name, atPrelude=prelude, atBlock=block}
                (nextInputToken:state') = state
        ((AtKeywordToken name):state) = s

consumeListOfRules :: Bool -> [CSSToken] -> ([Rule], [CSSToken])
consumeListOfRules topLevelFlag state = go [] state
    where
        go :: [Rule] -> [CSSToken] -> ([Rule], [CSSToken])
        go rules state = case nextInputToken of
            WhitespaceToken -> go rules state'
            EOFToken -> (rules, state)
            CDOToken -> qualify
            CDCToken -> qualify
            (AtKeywordToken _) -> let (val, state'') = consumeAtRule state in go (val:rules) state
            _ -> let (val, state'') = consumeQualifiedRule state in case val of
                (Just val) -> go (val:rules) state''
                Nothing -> go rules state''
            where
                qualify = if topLevelFlag
                    then go rules state'
                    else let (val, state'') = consumeQualifiedRule state in case val of
                        (Just val) -> go (val:rules) state''
                        Nothing -> go rules state''

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
        (nextInputToken:state') = eatWhitespace state

        eatWhitespace state = if nextInputToken == WhitespaceToken
            then eatWhitespace state'
            else state
            where (nextInputToken:state') = state

        eatVals :: [ComponentValue] -> [CSSToken] -> ([ComponentValue], [CSSToken])
        eatVals vals state = if nextInputToken == EOFToken
            then (vals, state)
            else let (val, state'') = consumeComponentValue state in eatVals (val:vals) state''
            where (nextInputToken:state') = state

        declar val imp = Declaration {declarationName=name, declarationValue=reverse val, declarationImportant=imp}

killUnknown state = if nextInputToken `elem` [SemicolonToken, EOFToken]
    then killUnknown $ snd $ consumeComponentValue state
    else state'
    where
        (nextInputToken:state') = state

consumeIdent :: [CSSToken] -> (Maybe Declaration, [CSSToken])
consumeIdent state = go [] state
    where 
        go :: [CSSToken] -> [CSSToken] -> (Maybe Declaration, [CSSToken])
        go tokens state = if nextInputToken `elem` [SemicolonToken, EOFToken]
            then let (val, _) = consumeDeclaration $ reverse (EOFToken:tokens) in case val of
                (Just val) -> (Just val, state)
                Nothing -> (Nothing, state')
            else go (nextInputToken:tokens) state'
            where (nextInputToken:state') = state

consumeStyleBlock state = go [] [] state
    where
        go decls rules state = case nextInputToken of
            WhitespaceToken -> go decls rules state'
            SemicolonToken -> go decls rules state'
            EOFToken -> decls ++ rules
            (AtKeywordToken _) -> let (val, state'') = consumeAtRule state in go decls (val:rules) state''
            (IdentToken _) -> let (val, state'') = consumeIdent state in case val of
                (Just val) -> go (DeclarationRule val:decls) rules state''
                Nothing -> go decls rules state''
            (DelimToken '&') -> let (val, state'') = consumeQualifiedRule state in case val of
                (Just val) -> go decls (val:rules) state''
                Nothing -> go decls rules state''
            _ -> go decls rules $ killUnknown state
            where
                (nextInputToken:state') = state

consumeListOfDeclarations :: [CSSToken] -> ([ComponentValue], [CSSToken])
consumeListOfDeclarations state = go [] state
    where
        go :: [ComponentValue] -> [CSSToken] -> ([ComponentValue], [CSSToken])
        go list [] = (list, [EOFToken])
        go list state = case nextInputToken of
            WhitespaceToken -> go list state'
            SemicolonToken -> go list state'
            EOFToken -> (list, state)
            (AtKeywordToken _) -> let (val, state'') = consumeAtRule state in go (RuleValue val:list) state''
            (IdentToken _) -> let (val, state'') = consumeIdent state in case val of
                (Just val) -> go (DeclarationValue val:list) state''
                Nothing -> go list state''
            _ -> go list $ killUnknown state
            where
                (nextInputToken:state') = state

_parseList :: [ComponentValue] -> [CSSToken] -> [ComponentValue]
_parseList out state = if nextInputToken == EOFToken
    then out
    else let (val, state') = consumeComponentValue state in _parseList (val:out) state'
    where nextInputToken = state!!0

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
    (PreservedValue (HashToken (id, n))) -> "#" ++ n
    (SimpleBlockValue (SimpleCurlyBlock (CurlyBlock t c))) -> foldr (\ t -> ((outList "" "" (map SelectorValue t) ++ ",") ++)) "" t ++ " {\n" ++ replaceAll "\n" "\n\t" (outList "\t" "" c) ++ "}\n\n"
    (SimpleBlockValue (SimpleSquareBlock (SquareBlock c))) -> "[" ++ outList "" "" c ++ "]"
    (SimpleBlockValue (SimpleParenthesisBlock (ParenthesisBlock c))) -> "(" ++ outList "" "" c ++ ")"
    (DeclarationValue (Declaration n v i)) -> n ++ ": " ++ outList "" " " v ++ (if i then "!important" else "") ++ ";\n"
    (SelectorValue (TagSelector s)) -> ' ' : s
    (SelectorValue (HashSelector s)) -> ' ' : s
    (SelectorValue (ClassSelector s)) -> ' ' : s
    (SelectorValue (StateSelector s l)) -> ' ' : s ++ ':' : l
    _ -> show val
    ) ++ delim) delim rest

