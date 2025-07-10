{-# LANGUAGE MultiWayIf #-}

module CSSParser where

import Text.Regex.TDFA
import Control.Applicative
import Data.Function
import Data.Char

import HTMLParser (tracer)

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

someParser (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> 
        let (Parser fany) = (manyParser $ Parser f) 
        in case fany rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right as) -> (rest', Right $ a : as)
    (rest, Left err) -> (rest, Left err)

try (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> (rest, Right a)
    (_, Left err) -> (stream, Left err)

satisfy f = Parser $ \ stream -> case stream of
    [] -> ("", Left "end of stream")
    (c:cs) 
        | f c -> (cs, Right c)
        | otherwise -> (cs, Left "did not satisfy")

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

checkWouldStartIdentSequence = Parser $ \ stream -> (stream, Right $
    if length stream < 3
        then False
        else let (f:s:t:rest) = stream in if
            | f == '-' -> if
                | checkMatches identy s -> True
                | s == '-' -> True
                | s == '\\' && t /= '\n' -> True
            | checkMatches startIdenty f -> True
            | f == '\\' && t /= '\n' -> True
            | otherwise -> False)
        where
            (Parser identy) = matchIdent
            (Parser startIdenty) = matchIdentStart
            checkMatches checker toCheck = case checker [toCheck] of
                    (_, Right _) -> True
                    _ -> False

consumeIdentSequence = manyParser $ matchIdent <|> characterEscape

consumeHash = dropFirst <$> matchChar '#' <*> (Parser $ \ stream ->
    let (Parser check) = matchIdent <|> characterEscape
    in case check stream of
        (_, Right _) -> doHashy stream
        (_, Left _) -> (tail stream, Right $ DelimToken $ head stream))
    where
        dropFirst _ a = a
        (Parser doHashy) = HashToken <$> ((,) <$> checkWouldStartIdentSequence <*> consumeIdentSequence)

parseString str =
    let preProcessed = preProcess str
    in parse (manyParser (consumeWhitespace <|> consumeString '"' <|> consumeHash <|> consumeString '\'')) preProcessed

