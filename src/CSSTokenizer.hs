{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultilineStrings #-}

module CSSTokenizer where

import Text.Regex.TDFA
import Control.Applicative
import Data.Function
import Data.Char

import HTMLParser (tracer)

type Error = String
newtype Parser a = Parser { parse :: String -> (String, Either Error a)}

instance Functor Parser where
    fmap f (Parser f2) = Parser $ \ stream -> case f2 stream of
        (rest, Right out) -> (rest, Right $ f out)
        (rest, Left err) -> (rest, Left err)

instance Applicative Parser where
    pure a = Parser (, Right a)
    Parser ff <*> Parser xx = Parser $ \ stream -> case ff stream of
        (rest, Left err) -> (rest, Left err)
        (rest, Right f) -> case xx rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right x) -> (rest', Right $ f x)

instance Alternative Parser where
    empty = Parser (, Left "empty")
    (<|>) = orElse
    many = manyParser
    some = someParser

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser f0) (Parser f1) = Parser $ \ stream -> case f0 stream of
    (rest, Right out) -> (rest, Right out)
    (_, Left _) -> f1 stream

manyParser :: Parser a -> Parser [a]
manyParser (Parser f) = Parser $ fix $ \ this stream -> case f stream of
    (_, Left _) -> (stream, Right [])
    (rest, Right a) -> case this rest of
        (rest', Left err) -> (rest', Left err)
        (rest', Right as) -> (rest', Right $ a : as)

parseUntil :: Parser a1 -> Parser a2 -> Parser [a1]
parseUntil (Parser f) (Parser end) = Parser $ fix $ \ this stream -> case end stream of
    (rest, Right _) -> (rest, Right [])
    (_, Left _) -> case f stream of
        (rest, Left err) -> (rest, Left err)
        (rest, Right a) -> case this rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right as) -> (rest', Right $ a : as)

someParser :: Parser a -> Parser [a]
someParser (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) ->
        let (Parser fany) = (manyParser $ Parser f)
        in case fany rest of
            (rest', Left err) -> (rest', Left err)
            (rest', Right as) -> (rest', Right $ a : as)
    (rest, Left err) -> (rest, Left err)

numParser :: (Eq t, Num t) => t -> Parser a -> Parser [a]
numParser num (Parser f) = Parser (go num)
    where
        go 0 stream = (stream, Right [])
        go num stream = case f stream of
            (rest, Right a) -> case go (num - 1) rest of
                (rest', Right as) -> (rest', Right $ a : as)
                (rest', Left err) -> (rest', Left err)
            (rest, Left err) -> (rest, Left err)

try :: Parser a -> Parser a
try (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> (rest, Right a)
    (_, Left err) -> (stream, Left err)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
    [] -> ("", Left "end of stream")
    (c:cs) -> if f c
        then (cs, Right c)
        else (cs, Left "did not satisfy")

pluralate :: Applicative f => f a -> f [a]
pluralate p = (:) <$> p <*> pure []

passes :: Parser a -> Parser Bool
passes (Parser f) = Parser $ \ stream -> case f stream of
    (_, Right _) -> (stream, Right True)
    (_, Left _) -> (stream, Right False)

replaceAll :: String -> String -> String -> String
replaceAll regex toInsert str =
    let
        (before, checked, after) = str =~ regex
    in if checked /= ""
        then before ++ toInsert ++ replaceAll regex toInsert after
        else before

killRegisteredNurse :: String -> String
killRegisteredNurse = replaceAll "\r\n" "\n"

consumeComment :: Parser CSSToken
consumeComment = dropAll <$> matchString "/*" <*> manyParser (matchNotString "*/") <*> matchString "*/"
    where dropAll _ _ _ = NothingToken

preProcess :: String -> String
preProcess str = map (\ c -> case c of -- also replace surrogates somehow
    '\r' -> '\n'
    '\f' -> '\n'
    '\0' -> '\xfffd'
    _ -> c) $ killRegisteredNurse str

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
    | DimensionToken (Float, String)
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
    | NothingToken
    | EOFToken
    deriving (Show, Eq, Ord)

matchChar :: Char -> Parser Char
matchChar c = satisfy (== c)

matchString :: String -> Parser String
matchString = foldr (\ c -> (<*>) ((:) <$> matchChar c)) (pure [])

matchNotString :: String -> Parser String
matchNotString str = Parser $ \ stream -> case eater stream of
    (rest, Right a) -> if a == str
        then (rest, Left "not match")
        else (drop 1 stream, Right a)
    (rest, Left err) -> (rest, Left err)
    where (Parser eater) = numParser (length str) (satisfy $ const True)

matchWhitespace :: Parser Char
matchWhitespace = matchChar '\n' <|> matchChar '\t' <|> matchChar ' '

matchDigit :: Parser Char
matchDigit = satisfy (`elem` "0123456789")

matchUppercaseLetter :: Parser Char
matchUppercaseLetter = satisfy (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

matchLowercaseLetter :: Parser Char
matchLowercaseLetter = satisfy (`elem` "abcdefghijklmnopqrstuvwxyz")

matchLetter :: Parser Char
matchLetter = matchUppercaseLetter <|> matchLowercaseLetter

matchNonAscii :: Parser Char
matchNonAscii = satisfy (\ c -> ord c >= 0x80)

matchIdentStart :: Parser Char
matchIdentStart = matchLetter <|> matchNonAscii <|> matchChar '_' <|> matchChar '.'

matchIdent :: Parser Char
matchIdent = matchIdentStart <|> matchDigit <|> matchChar '-'

matchHex :: Parser Char
matchHex = satisfy (`elem` "0123456789abcdefABCDEF")

consumeWhitespace :: Parser CSSToken
consumeWhitespace = whitespace <$> someParser matchWhitespace
    where whitespace _ = WhitespaceToken

characterEscape :: Parser Char
characterEscape = dropFirst <$>
    matchChar '\\'
    <*> (matchChar '\n'
        <|> readIt <$> someParser matchHex
        <|> satisfy (const True))
    where
        dropFirst _ a = a
        readIt str =
            let readed = read $ "0x" ++ str
            in if readed == 0
                then '\xfffd'
                else chr readed

eatStringInsides :: Char -> Parser CSSToken
eatStringInsides endingCodePoint = Parser $ \ stream -> case go stream of
        (rest, Left _) -> (rest, Right BadStringToken)
        (rest, Right a) -> (rest, Right $ StringToken $ filter (/= '\0') a)
    where
        (Parser regulari) = characterEscape <|> satisfy (/= endingCodePoint)
        go stream = case stream of
            [] -> ("", Right [])
            ('\n':rest) -> (rest, Left "newline")
            _ -> case regulari stream of
                (_, Left _) -> (stream, Right [])
                (rest, Right a) -> case go rest of
                    (rest', Right as) -> (rest', Right $ a : as)
                    unreachable -> unreachable

consumeString :: Char -> Parser CSSToken
consumeString endingCodePoint = dropDels <$>
    matchChar endingCodePoint <*>
    eatStringInsides endingCodePoint <*>
    makeOptional (pluralate $ matchChar endingCodePoint)
    where dropDels _ a _ = a

matchEscape :: Parser [Char]
matchEscape = sequentiate (:) [matchChar '\\', satisfy (/= '\n')]

checkWouldStartIdentSequence :: Parser [Char]
checkWouldStartIdentSequence = sequentiate (++) [
    matchString "-"
    , pluralate matchIdentStart <|> matchString "-" <|> matchEscape
    ]
    <|> pluralate matchIdentStart
    <|> matchEscape

consumeIdentSequence :: Parser [Char]
consumeIdentSequence = manyParser $ characterEscape <|> matchIdent

consumeHash :: Parser CSSToken
consumeHash = dropFirst <$> matchChar '#' <*> Parser (\ stream ->
    let (Parser check) = matchIdent <|> characterEscape
    in case check stream of
        (_, Right _) -> doHashy stream
        (rest, Left err) -> (rest, Left err))
    where
        dropFirst _ a = a
        (Parser doHashy) = HashToken <$> ((,) <$> passes checkWouldStartIdentSequence <*> consumeIdentSequence)

consume :: Functor f => (t -> f a) -> b -> t -> f b
consume matchy out char = mhm <$> matchy char
    where mhm _ = out

consumeCharacter :: b -> Char -> Parser b
consumeCharacter = consume matchChar

sequentiate :: Applicative f => (a1 -> [a2] -> [a2]) -> [f a1] -> f [a2]
sequentiate f = foldr (\ p -> (<*>) (f <$> p)) (pure [])

makeOptional :: Parser String -> Parser String
makeOptional (Parser f) = Parser $ \ stream -> case f stream of
    (rest, Right a) -> (rest, Right a)
    (_, Left _) -> (stream, Right "")

killMe :: Parser a -> Parser a -> Parser a
killMe (Parser other) (Parser this) = Parser $ \ stream -> case this stream of
    (rest, Right a) -> (rest, Right a)
    (rest, Left _) -> other rest

checkDigit :: Parser Float
checkDigit = ready (sequentiate (++) [
    makeOptional (matchString "+" <|> matchString "-")
    , orAnd (someParser matchDigit) ((:) <$> matchChar '.' <*> someParser matchDigit)
    , makeOptional $ sequentiate (++) [
        matchString "e" <|> matchString "E"
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
        orAnd (Parser f) (Parser g) = Parser $ \ stream -> case f stream of
            (rest, Right a) -> case g rest of
                (rest', Right b) -> (rest', Right $ a ++ b)
                (_, Left err) -> (rest, Right a)
            (_, Left _) -> g stream

consumeNumeric :: Parser CSSToken
consumeNumeric =
    (DimensionToken <$> ((,) <$> checkDigit <*> eatByStart checkWouldStartIdentSequence consumeIdentSequence))
    <|> (PercentageToken <$> (dropLast <$> checkDigit <*> matchChar '%'))
    <|> (NumberToken <$> checkDigit)
    where dropLast a _ = a

consumeUrl :: Parser CSSToken
consumeUrl = killMe (dropAll <$> manyParser (characterEscape <|> satisfy (/= ')')) <*> matchChar ')') $ UrlToken <$> parseUntil
    (characterEscape <|> satisfy (`notElem` "'\"("))
    ((++) <$> manyParser matchWhitespace <*> matchString ")")
    where dropAll _ _ = BadUrlToken

consumeIdentLike :: Parser CSSToken
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
            , concat <$> manyParser (numParser 2 matchWhitespace)
            , makeOptional $ pluralate matchWhitespace
            ]
        dropLast a _ = a

eatByStart :: Parser a1 -> Parser a2 -> Parser a2
eatByStart (Parser check) (Parser consumer) = Parser $ \ stream -> case check stream of
    (_, Right _) -> consumer stream
    (rest, Left err) -> (rest, Left err)

postProcess :: [CSSToken] -> [CSSToken]
postProcess out = filter (not . (`elem` [WhitespaceToken, NothingToken])) $ out ++ [EOFToken]

basics :: String
basics = """
b {
    font-weight: bold;
}

tt {
    font-style: italic;
}
"""

parseString :: String -> (String, Either Error [CSSToken])
parseString str = 
    let preProcessed = basics ++ preProcess str
    in parse (postProcess <$> manyParser (
                consumeComment
                <|> consumeWhitespace
                <|> consumeString '"'
                <|> consumeHash
                <|> consumeString '\''
                <|> consumeCharacter OpeningParenthesisToken '('
                <|> consumeCharacter ClosingParenthesisToken ')'
                <|> consumeNumeric
                <|> consumeCharacter CommaToken ','
                <|> consume matchString CDCToken "->"
                <|> consumeCharacter ColonToken ':'
                <|> consumeCharacter SemicolonToken ';'
                <|> consume matchString CDOToken "<!--"
                <|> AtKeywordToken <$> eatByStart ((:) <$> matchChar '@' <*> checkWouldStartIdentSequence) ((:) <$> matchChar '@' <*> consumeIdentSequence)
                <|> eatByStart matchEscape consumeIdentLike
                <|> consumeCharacter OpeningSquareBracketToken '['
                <|> consumeCharacter ClosingSquareBracketToken ']'
                <|> consumeCharacter OpeningCurlyBracketToken '{'
                <|> consumeCharacter ClosingCurlyBracketToken '}'
                <|> eatByStart matchIdentStart consumeIdentLike
                <|> Parser (\case
                    [] -> ("", Left "end of stream")
                    (c:str) -> (str, Right $ DelimToken c))
            )) preProcessed

