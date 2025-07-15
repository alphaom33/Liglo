{-# LANGUAGE MultiWayIf #-}

module CSSParser where

import Text.Regex.TDFA
import Control.Applicative
import Data.Function
import Data.Char

import HTMLParser (tracer)

import Control.Monad (return)
import Control.Exception (catch)

type Error = String
data Parser a = Parser { parse :: String -> (String, Either Error a)}

instance Functor Parser where
    fmap f (Parser f2) = Parser $ \ stream -> case f2 stream of
        (rest, Right out) -> (rest, Right $ f out)
        (rest, Left err) -> (rest, Left err)

instance Applicative Parser where
    pure a = Parser $ \ stream -> (stream, Right a)
    Parser ff <*> Parser xx = Parser $ \ stream -> case ff stream of
        (rest, Left err) -> (rest, Left err)
        (rest, Right f) -> case xx rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right x) -> (rest', Right $ f x)

instance Alternative Parser where
    empty = Parser $ \ stream -> (stream, Left "empty")
    (<|>) = orElse
    many = manyParser
    some = someParser

orElse (Parser f0) (Parser f1) = Parser $ \ stream -> case f0 stream of
    (rest, Right out) -> (rest, Right out)
    (rest, Left _) -> f1 stream

manyParser (Parser f) = Parser $ fix $ \ this stream -> case f stream of
    (_, Left _) -> (stream, Right [])
    (rest, Right a) -> case this rest of
        (rest', Left err) -> (rest', Left err)
        (rest', Right as) -> (rest', Right $ a : as)

parseUntil (Parser f) (Parser end) = Parser $ fix $ \ this stream -> case end stream of
    (rest, Right _) -> (rest, Right [])
    (_, Left err) -> case f stream of
        (rest, Left err) -> (rest, Left err)
        (rest, Right a) -> case this rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right as) -> (rest', Right $ a : as)

someParser (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> 
        let (Parser fany) = (manyParser $ Parser f) 
        in case fany rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right as) -> (rest', Right $ a : as)
    (rest, Left err) -> (rest, Left err)

numParser num (Parser f) = Parser (go num)
    where 
        go 0 stream = (stream, Right [])
        go num stream = case f stream of
            (rest, Right a) -> case go (num - 1) rest of
                (rest', Right as) -> (rest', Right $ a : as)
                (rest', Left err) -> (rest', Left err)
            (rest, Left err) -> (rest, Left err)

try (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> (rest, Right a)
    (_, Left err) -> (stream, Left err)

satisfy f = Parser $ \ stream -> case stream of
    [] -> ("", Left "end of stream")
    (c:cs) 
        | f c -> (cs, Right c)
        | otherwise -> (cs, Left "did not satisfy")

pluralate p = ((:) <$> p <*> pure [])

passes (Parser f) = Parser $ \ stream -> case f stream of
    (_, Right _) -> (stream, Right True)
    (_, Left _) -> ("", Right False)

replaceAll :: String -> String -> String -> String
replaceAll regex toInsert str =
    let
        (before, checked, after) = str =~ regex
    in if checked /= ""
        then before ++ toInsert ++ replaceAll regex toInsert after
        else before

killRegisteredNurse :: String -> String
killRegisteredNurse = replaceAll "\r\n" "\n"

killComments :: String -> String
killComments = replaceAll "/\\*.*\\*/" ""

preProcess str = map (\ c -> case c of -- also replace surrogates somehow
    '\r' -> '\n'
    '\f' -> '\n'
    '\0' -> '\xfffd'
    _ -> c) $ killComments $ killRegisteredNurse str

data CSSToken =
    IdentToken String
    | FunctionToken String
    | AtKeywordToken String
    | HashToken (Bool, String)
    | StringToken String
    | BadStringToken
    | UrlToken String
    | BadUrlToken
    | DelimToken Char
    | NumberToken Float
    | PercentageToken Float
    | DimensionToken Float
    | WhitespaceToken
    | CDOToken
    | CDCToken
    | ColonToken
    | SemicolonToken
    | CommaToken
    | OpeningSquareBracketToken
    | ClosingSquareBracketToken
    | OpeningParenthesisToken
    | ClosingParenthesisToken
    | OpeningCurlyBracketToken
    | ClosingCurlyBracketToken
    deriving (Show, Eq)

matchChar :: Char -> Parser Char
matchChar c = satisfy (== c)

matchString :: String -> Parser String
matchString [] = pure []
matchString (c:cs) = (:) <$> matchChar c <*> matchString cs

matchWhitespace :: Parser Char
matchWhitespace = matchChar '\n' <|> matchChar '\t' <|> matchChar ' '

matchDigit = satisfy (`elem` "0123456789")
matchUppercaseLetter = satisfy (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
matchLowercaseLetter = satisfy (`elem` "abcdefghijklmnopqrstuvwxyz")
matchLetter = matchUppercaseLetter <|> matchLowercaseLetter
matchNonAscii = satisfy (\ c -> ord c >= 0x80)
matchIdentStart = matchLetter <|> matchNonAscii <|> matchChar '_'
matchIdent = matchIdentStart <|> matchDigit <|> matchChar '-'

consumeWhitespace :: Parser CSSToken
consumeWhitespace = whitespace <$> someParser matchWhitespace
    where whitespace _ = WhitespaceToken

characterEscape = 
    let 
        (Parser backed) = matchChar '\\'
        (Parser new) = matchChar '\n'
        (Parser hex) = someParser (satisfy (`elem` "0123456789abcdef"))
    in Parser $ \ stream -> case backed stream of
        (rest, Right a) -> case new rest of
            (rest', Right b) -> (rest', Right '\0')
            (_, Left _) -> case hex rest of
                (rest'', Right code) ->
                    let
                        translated = read $ "0x" ++ code
                        (Parser white) = try matchWhitespace
                    in if translated == 0
                        then (fst $ white rest'', Right '\xfffd')
                        else (fst $ white rest'', Right $ chr translated)
                (_, Left _) -> (tail rest, Right $ head rest)
        (rest, Left err) -> (rest, Left err)

eatStringInsides :: Char -> Parser CSSToken
eatStringInsides endingCodePoint = Parser $ \ stream -> case go stream of
        (rest, Left _) -> (rest, Right BadStringToken)
        (rest, Right a) -> (rest, Right $ StringToken $ filter (/= '\0') a)
    where 
        (Parser regulari) = characterEscape <|> satisfy (/= endingCodePoint)
        go stream = if head stream == '\n'
            then (tail stream, Left "newline")
            else case regulari stream of
                (rest, Left err) -> (stream, Right [])
                (rest, Right a) -> case go rest of
                    (rest', Right as) -> (rest', Right $ a : as)
                    unreachable -> unreachable

consumeString :: Char -> Parser CSSToken
consumeString endingCodePoint = dropDels <$> matchChar endingCodePoint <*> eatStringInsides endingCodePoint <*> matchChar endingCodePoint
    where dropDels _ a _ = a

matchEscape = sequentiate (:) [matchChar '\\', satisfy (/= '\n')]

checkWouldStartIdentSequence = sequentiate (++) [
    matchString "-"
    , pluralate matchIdentStart <|> matchString "-" <|> matchEscape
    ]
    <|> pluralate matchIdentStart
    <|> matchEscape

consumeIdentSequence = manyParser $ matchIdent <|> characterEscape

consumeHash = dropFirst <$> matchChar '#' <*> (Parser $ \ stream ->
    let (Parser check) = matchIdent <|> characterEscape
    in case check stream of
        (_, Right _) -> doHashy stream
        (_, Left _) -> (tail stream, Right $ DelimToken $ head stream))
    where
        dropFirst _ a = a
        (Parser doHashy) = HashToken <$> ((,) <$> passes checkWouldStartIdentSequence <*> consumeIdentSequence)

consume matchy out char = mhm <$> matchy char
    where mhm _ = out

consumeCharacter = consume matchChar

sequentiate f [] = pure []
sequentiate f (p:ps) = (f) <$> p <*> sequentiate f ps

makeOptional (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> (rest, Right a)
    (_, Left _) -> (stream, Right "")

killMe (Parser other) (Parser this) = Parser $ \ stream -> case this stream of
    (rest, Right a) -> (rest, Right a)
    (rest, Left _) -> other rest

checkDigit = NumberToken <$> ready (sequentiate (++) [
    makeOptional (matchString "+" <|> matchString "-")
    , wonky (someParser matchDigit) ((:) <$> matchChar '.' <*> someParser matchDigit)
    , makeOptional $ sequentiate (++) [
        (matchString "e" <|> matchString "E")
        , makeOptional (matchString "+" <|> matchString "-") 
        , someParser matchDigit
        ]
    ])
    where 
        ready (Parser f) = Parser $ \ stream -> case f stream of
            (rest, Right []) -> (rest, Left "")
            (rest, Right a) -> (rest, Right $ read $ go a)
            (rest, Left err) -> (rest, Left err)

            where go a = case a of
                    ('.':str) -> go $ "0." ++ str
                    ('+':str) -> go str
                    _ -> a


        wonky (Parser f) (Parser g) = Parser $ \ stream -> case f stream of
            (rest, Right a) -> case g rest of
                (rest', Right b) -> (rest', Right $ a ++ b)
                (_, Left err) -> (rest, Right a)
            (_, Left _) -> g stream

consumeUrl = killMe (dropAll <$> manyParser (characterEscape <|> satisfy (/= ')')) <*> matchChar ')') $ UrlToken <$> parseUntil 
    (characterEscape <|> satisfy (\ c -> not $ c `elem` "'\"("))
    ((++) <$> manyParser matchWhitespace <*> matchString ")")
    where dropAll _ _ = BadUrlToken

consumeIdentLike = 
    urly
    <|> FunctionToken <$> (dropLast <$> consumeIdentSequence <*> matchChar '(')
    <|> IdentToken <$> consumeIdentSequence
    where 
        urly = Parser $ \ stream -> case beginn stream of
            (rest, Right _) -> case strart rest of
                (_, Right _) -> (rest, Right $ FunctionToken "url")
                (_, Left _) -> parse consumeUrl rest
            (rest, Left err) -> (rest, Left err)
        (Parser strart) = matchChar '\'' <|> matchChar '"'
        (Parser beginn) = sequentiate (++) [
            matchString "url("
            , foldr (++) "" <$> manyParser (numParser 2 matchWhitespace)
            , makeOptional $ pluralate matchWhitespace
            ]
        dropLast a _ = a

eatByStart (Parser check) (Parser consumer) = Parser $ \ stream -> case check stream of
    (_, Right _) -> consumer stream
    (rest, Left err) -> (rest, Left err)

parseString str =
    let preProcessed = preProcess str
    in parse (manyParser (
                consumeWhitespace 
                <|> consumeString '"' 
                <|> consumeHash 
                <|> consumeString '\'' 
                <|> consumeCharacter OpeningParenthesisToken '('
                <|> consumeCharacter ClosingParenthesisToken ')'
                <|> checkDigit -- ahhhhhhhhh
                <|> consumeCharacter CommaToken ','
                <|> consume matchString CDCToken "->"
                <|> consumeCharacter ColonToken ':'
                <|> consumeCharacter SemicolonToken ';'
                <|> consume matchString CDOToken "<!--"
                <|> AtKeywordToken <$> eatByStart ((:) <$> matchChar '@' <*> checkWouldStartIdentSequence) consumeIdentSequence
                <|> eatByStart matchEscape consumeIdentLike
                <|> consumeCharacter OpeningSquareBracketToken '['
                <|> consumeCharacter ClosingSquareBracketToken ']'
                <|> consumeCharacter OpeningCurlyBracketToken '{'
                <|> consumeCharacter ClosingCurlyBracketToken '}'
                <|> eatByStart matchIdentStart consumeIdentLike
                <|> Parser (\ stream -> case stream of
                    [] -> ("", Left "end of stream")
                    (c:str) -> (str, Right $ DelimToken c))
            )) preProcessed

