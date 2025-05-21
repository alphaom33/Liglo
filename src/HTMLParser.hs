{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module HTMLParser where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Debug.Trace (trace)
import Control.Monad.Fix (fix)

import Lens.Micro.Mtl (view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, (&), (%~))
import Data.Char (toUpper, toLower)
import qualified Data.Set as S
import Data.Either (isRight)

tracer :: Show a => a -> a
tracer a = trace (show a) a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \case
  []                 -> ([], Left "end of stream")
  (c:cs) | f c       -> (cs, Right c)
         | otherwise -> (cs, Left $ show c ++ "did not satisfy")

try :: Parser a -> Parser a
try (P f) = P $ \stream0 -> case f stream0 of
  (_      , Left err) -> (stream0, Left err)
  (stream1, Right a ) -> (stream1, Right a )

whoCares :: Parser a -> Parser b -> Parser b
whoCares (P f) (P f1) = P $ \stream0 -> case f stream0 of
  (_      , Left _) -> f1 stream0
  (stream1, Right _) -> f1 stream1

consumeUntil :: Parser a -> Parser a
consumeUntil (P f) = P $ fix $ \me stream -> case f stream of
    ("", Left a) -> ("", Left a)
    (_, Left _) -> me (drop 1 stream)
    (rest, Right a) -> (rest, Right a)

orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left _) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)

tryElse, (<||>) :: Parser a -> Parser a -> Parser a
tryElse f1 f2 = try f1 <|> try f2
(<||>) = tryElse

type Error = String
newtype Parser a = P { parse :: String -> (String, Either Error a) }

instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

instance Applicative Parser where
  pure a = P (, Right a)
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)

instance Alternative Parser where
  empty = P (, Left "empty")
  (<|>) = orElse

  many = manyParser
  some = someParser

-- | 0 or more
manyParser :: Parser a -> Parser [a]
manyParser (P f) = P go where
  go stream = case f stream of
    (_      , Left _) -> (stream, Right [])  -- throws away the error
    (stream', Right a ) -> case go stream' of
      (streamFin, Left err) -> (streamFin, Left err)
      (streamFin, Right as) -> (streamFin, Right (a : as))

-- | 1 or more
someParser :: Parser a -> Parser [a]
someParser (P f) = P $ \stream -> case f stream of
  (stream', Left err) -> (stream', Left err)
  (stream', Right a ) ->
    let (P fmany) = manyParser (P f)
    in case fmany stream' of
      (stream'', Left err) -> (stream'', Left err)
      (stream'', Right as) -> (stream'', Right (a:as))

doSequentialString :: [Parser String] -> Parser String
doSequentialString ffs = foldr (\ a -> (<*>) ((++) <$> a)) (pure []) ffs

dropLast :: [String] -> String -> String
dropLast a _ = foldl (++) "" a

matchThrough :: String -> Parser String
matchThrough str = P $ fix $ \ me stream ->
  let (P doMatch) = matchString str
  in case doMatch stream of
    (stream', Right a) -> (stream', Right a)
    (_, Left _) -> me $ tail stream

discardDels :: a -> b -> c -> b
discardDels _ a _ = a

char :: Char -> Parser Char
char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toLower c) <||> char (toUpper c)

matchString :: String -> Parser String
matchString = foldr (\ c -> (<*>) ((:) <$> char c)) (pure [])

matchNotString :: String -> Parser String
matchNotString = foldr (\ c -> (<*>) ((:) <$> notChar c)) (pure [])

matchStringIgnoreCase :: String -> Parser String
matchStringIgnoreCase = foldr (\ c -> (<*>) ((:) <$> charIgnoreCase c)) (pure [])

matchTagIgnoreCase :: String -> Parser String
matchTagIgnoreCase str = discardDels <$> char '<' <*> matchStringIgnoreCase str <*> char '>'

checkDels :: String -> [Char] -> (String, Either Error String)
checkDels stream [] = (stream, Left "Expected ")
checkDels stream (del:dels) =
    let
        (P op) = char del
    in case op stream of
        (_, Left _) -> checkDels stream dels
        (stream', Right _) -> (stream', Right (del:[]))

string :: Parser String
string = P $ \stream -> 
    let
        dels = "'\""
    in case checkDels stream dels of
        (stream', Right del) -> let (P end) = matchThrough del in end stream'
        (stream', Left err) -> (stream', Left $ err ++ dels)

data Attribute = Attribute (String, String) deriving (Show, Eq, Ord)

data ActiveFormattingTagType =  Applet | Object | Marquee | Template | Td | Th | Caption deriving (Show, Eq)

data TagType = 
  ActiveFormattingTagType ActiveFormattingTagType |
  Select |
  Table |
  Tr |
  TBody |
  THead |
  TFoot |
  Colgroup |
  Head |
  Body |
  FrameSet |
  Html |
  External |
  Area |
  Base |
  Br |
  Col |
  Embed |
  Hr |
  Input |
  Link |
  Meta |
  Source |
  Track |
  Wbr |
  Script |
  Style |
  TextArea |
  Title |
  SVG |
  Address |
  Article |
  Aside |
  Basefont |
  Bgsound |
  Blockquote |
  Button |
  Center |
  Dd |
  Details |
  Dir |
  Div |
  Dl |
  Dt |
  Fieldset |
  Figcaption |
  Figure |
  Footer |
  Form |
  Frame |
  Frameset |
  H1 |
  H2 |
  H3 |
  H4 |
  H5 |
  H6 |
  Header |
  Hgroup |
  Iframe |
  Img |
  Keygen |
  Li |
  Listing |
  Main |
  Menu |
  Nav |
  Noembed |
  Noframes |
  Noscript |
  Ol |
  Param |
  Plaintext |
  Pre |
  Search |
  Section |
  Summary |
  Tbody |
  Textarea |
  Tfoot |
  Thead |
  Ul |
  Xmp |
  MathMLMi |
  MathMLMo |
  MathMLMn |
  MathMLMs |
  MathMLMtext |
  MathMLAnnotationXml |
  SVGForeignObject |
  SVGDesc |
  SVGTitle |
  A |
  B |
  Big |
  Code |
  Em |
  Font |
  I |
  Nobr |
  S |
  Small |
  Strike |
  Strong |
  Tt |
  U |
  PTag |
  TestType deriving (Show, Eq)

data Category = Special | Formatting | Ordinary
specials = [
        Address,
        Applet,
        Area,
        Article,
        Aside,
        Base,
        Basefont,
        Bgsound,
        Blockquote,
        Body,
        Br,
        Button,
        Caption,
        Center,
        Col,
        Colgroup,
        Dd,
        Details,
        Dir,
        Div,
        Dl,
        Dt,
        Embed,
        Fieldset,
        Figcaption,
        Figure,
        Footer,
        Form,
        Frame,
        Frameset,
        H1,
        H2,
        H3,
        H4,
        H5,
        H6,
        Head,
        Header,
        Hgroup,
        Hr,
        Html,
        Iframe,
        Img,
        Input,
        Keygen,
        Li,
        Link,
        Listing,
        Main,
        Marquee,
        Menu,
        Meta,
        Nav,
        Noembed,
        Noframes,
        Noscript,
        Object,
        Ol,
        PTag,
        Param,
        Plaintext,
        Pre,
        Script,
        Search,
        Section,
        Select,
        Source,
        Style,
        Summary,
        Table,
        Tbody,
        Td,
        Template,
        Textarea,
        Tfoot,
        Th,
        Thead,
        Title,
        Tr,
        Track,
        Ul,
        Wbr,
        Xmp,
        MathMLMi,
        MathMLMo,
        MathMLMn,
        MathMLMs,
        MathMLMtext,
        MathMLAnnotationXml,
        SVGForeignObject,
        SVGDesc,
        SVGTitle
    ]
formattings = [
  A,
  B,
  Big,
  Code,
  Em,
  Font,
  I,
  Nobr,
  S,
  Small,
  Strike,
  Strong,
  Tt,
  U
    ]

data ElementKind = Void | TheTemplate | RawText | EscapableText | Foreign | Normal deriving (Show, Eq)
data Scope = InScope | InListScope | InButtonScope | InTableScope | InSelectScope deriving (Show, Eq)

data Tag = Tag {
    tagType :: TagType
    , nameSpace :: String
    , attrs :: [Attribute]
    , selfClosing :: Bool
} deriving (Show, Eq)

getTagCategory :: TagType -> Category
getTagCategory tagType = 
    if tagType `elem` specials then Special
    else if tagType `elem` formattings then Formatting
    else Ordinary

-- getTagScope :: TagType -> Scope
-- getTagScope tagType
--     | tagType `elem` [
--         Applet,
--         Caption,
--         Html,
--         Table,
--         Td,
--         Th,
--         Marquee,
--         Object,
--         Template,
--         MathMLMi,
--         MathMLMo,
--         MathMLMn,
--         MathMLMs,
--         MathMLMtext,
--         MathMLAnnotationXml,
--         SVGForeignObject,
--         SVGDesc,
--         SVGTitle
--         ] = InScope 
--     | tagType `elem` [Ol, Ul] = InListScope 
--     | tagType `elem` [] = InButtonScope 
--     | tagType `elem` [] = InTableScope 
--     | tagType `elem` [] = InSelectScope


getTagKind :: TagType -> ElementKind
getTagKind tagType = case tagType of
  Area -> Void
  Base -> Void
  Br -> Void
  Col -> Void
  Embed -> Void
  Hr -> Void
  Img -> Void
  Input -> Void
  Link -> Void
  Meta -> Void
  Source -> Void
  Track -> Void
  Wbr -> Void
  Template -> TheTemplate
  Script -> RawText
  Style -> RawText
  TextArea -> EscapableText
  Title -> EscapableText
  SVG -> Foreign
  _ -> Normal

strToType :: String -> TagType
strToType str = case map toLower str of
  "select" -> Select
  "template" -> Template
  "table" -> Table
  "td" -> Td
  "th" -> Th
  "tr" -> Tr
  "tbody" -> TBody
  "thead" -> THead
  "tfoot" -> TFoot
  "caption" -> Caption
  "colgroup" -> Colgroup
  "head" -> Head
  "body" -> Body
  "frameset" -> FrameSet
  "html" -> Html
  "area" -> Area
  "base" -> Base
  "br" -> Br
  "col" -> Col
  "embed" -> Embed
  "hr" -> Hr
  "img" -> Img
  "input" -> Input
  "link" -> Link
  "meta" -> Meta
  "source" -> Source
  "track" -> Track
  "wbr" -> Wbr
  "script" -> Script
  "style" -> Style
  "svg" -> SVG
  _ -> External

makeTag :: String -> [Attribute] -> Tag
makeTag name' attrs' = Tag {tagType=strToType name', attrs=attrs', selfClosing=False}

comment :: Parser String
comment = (++) <$> matchString "<!--" <*> matchThrough "-->"

whitespace :: Parser Char
whitespace = char '\t' <||> char '\n' <||> char '\f' <||> char '\r' <||> char ' '

manyspace :: Parser String
manyspace = manyParser whitespace
somespace :: Parser String
somespace = someParser whitespace

attribute :: Parser Attribute
attribute = P $ \cs -> (cs, Left "a")

checkSelfClosing tag = P $ \stream ->
  let (P solidus) = char '/'
  in case solidus stream of
    (stream', Right _) -> (stream', Right Tag {
      tagType=tagType tag
      , attrs = attrs tag
      , selfClosing = True
    })
    (stream', Left err) -> (stream, Right tag)

alpha = satisfy $ \ c -> (toLower c) `elem` "abcdefghijklmnopqrstuvwxyz"
digit = satisfy (`elem` "0123456789")
alphanumeric = alpha <||> digit

tagName = manyParser alphanumeric

startTag :: Parser Tag
startTag = P $ \stream ->
  let 
    dropFirst _ a = a
    dropDels _ a _ = a
    (P easyPart) = dropDels <$> char '<' <*> (makeTag <$> tagName <*> manyParser attribute) <*> manyspace
  in case easyPart stream of
    (stream', Right tag) -> let kind = getTagKind $ tagType tag in
      if kind == Void then let (P close) = whoCares (char '/') (char '>') in end tag $ close stream'
      else if kind == Foreign then let (P close) = checkSelfClosing tag in close stream'
      else let (P gt) = char '>' in end tag $ gt stream'
    (stream', Left err) -> (stream', Left err)
  where
    what tag (stream', res) = case res of
      Right _ -> (stream', Right tag)
      Left err -> (stream', Left err)
    end tag res = case res of
      (stream', Right _) -> (stream', Right tag)
      (stream', Left err) -> (stream', Left err)

legacy :: Parser String
legacy = doSequentialString [somespace, matchStringIgnoreCase "SYSTEM", somespace, (matchStringIgnoreCase "'about:legacy-compat'" <||> matchStringIgnoreCase "\"about:legacy-compat\"")]

doctype :: Parser String
doctype = doSequentialString [matchStringIgnoreCase "<!DOCTYPE", somespace, matchStringIgnoreCase "html", whoCares legacy $ manyspace, matchString ">"]

preProcess :: String -> String
preProcess str = filter (/= '\r') str

data InsertionMode = Initial | InSelect | InSelectInTable | InCell | InRow | InTableBody | InCaption | InColgroup | InTable | InHead | InBody | InFrameSet | BeforeHead | AfterHead | TestMode deriving (Show, Eq)

data StateMachineState = DataState | CharacterReferenceState | TagOpenState | RCDataState | RawTextState | RCDataLessThanSignState | ScriptDataState | ScriptDataLessThanSignState | PlainTextState | MarkupDeclarationOpenState | EndTagOpenState | RawTextLessThanSignState deriving Show

data State = State {
    _stateMachineState :: StateMachineState
    , _returnState :: StateMachineState
    , _input :: String
    , _currentInputCharacter :: Char
    , _openElements :: [Tag]
    , _activeFormattingElements :: [Tag]
    , _mode :: InsertionMode
    , _templateModes :: [InsertionMode]
    , _headPointer :: Maybe Tag
    , _formPointer :: Maybe Tag
    , _scriptingEnabled :: Bool
    , _framesetOk :: Bool
    , _reconsume :: Bool
} deriving (Show)
$(makeLenses ''State)

data ActiveFormattingElement = ActiveFormattingElement ActiveFormattingTagType | Marker deriving Show

data InputToken = Ampersand | LessThan | Null | EOF deriving Show

getNextInputCharacter :: State -> (State, Char)
getNextInputCharacter state = (state, '&')

emitToken :: Char -> State -> State
emitToken char state = state

doStateMachine state = case _stateMachineState state of
    DataState -> case nextInputCharacter of
        '&' -> set returnState DataState (set stateMachineState CharacterReferenceState state')
        '<' -> set stateMachineState TagOpenState state'
        '\0' -> emitToken (_currentInputCharacter state') state'
        '#' -> emitToken '\x0003' state'
        _ -> emitToken (_currentInputCharacter state') state'
    RCDataState -> case nextInputCharacter of
        '&' -> set returnState RCDataState (set stateMachineState CharacterReferenceState state')
        '<' -> set stateMachineState RCDataLessThanSignState state'
        '\0' -> emitToken '\xfffd' state'
        '#' -> emitToken '\x0003' state'
        _ -> emitToken (_currentInputCharacter state') state'
    RawTextState -> case nextInputCharacter of
        '<' -> set stateMachineState RawTextLessThanSignState state'
        '\0' -> emitToken '\xfffd' state'
        '#' -> emitToken '\x0003' state'
        _ -> emitToken (_currentInputCharacter state') state'
    ScriptDataState -> case nextInputCharacter of
        '<' -> set stateMachineState ScriptDataLessThanSignState state'
        '\0' -> emitToken '\xfffd' state'
        '#' -> emitToken '\x0003' state'
        _ -> emitToken (_currentInputCharacter state') state'
    PlainTextState -> case nextInputCharacter of
        '\0' -> emitToken '\xfffd' state'
        '#' -> emitToken '\x0003' state'
        _ -> emitToken (_currentInputCharacter state') state'
    TagOpenState
        | nextInputCharacter == '!' -> set stateMachineState MarkupDeclarationOpenState state'
        | nextInputCharacter == '/' -> set stateMachineState EndTagOpenState state' 
        | isRight $ snd $ parse alpha $ nextInputCharacter:[] -> state'
        | nextInputCharacter == '?' -> state'
        | nextInputCharacter == '#' -> state'
    _ -> state
    where (state', nextInputCharacter) = getNextInputCharacter state


pushToActiveFormatting :: State -> Tag -> State
pushToActiveFormatting state tag =
    let
        elements = _activeFormattingElements state
        countTags = foldl (\ a b -> a + if S.fromList (attrs b) == S.fromList (attrs tag) then 1 else 0) 0 elements 
        elements' = 
            if countTags >= 3 then drop 1 elements
            else elements
    in
        set activeFormattingElements (tag:elements') state

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

reconstructActiveFormatting :: State -> State
reconstructActiveFormatting state
    | length els == 0 = state
    | els!!0 == Marker = state
    | els!!0 `elem` (_openElements state) = state
    | _ = rewind 0
    where 
        els = (_activeFormattingElements state)
        check el = els!!el != Marker && not (els!!el `elem` (_openElements))
        rewind el 
            | el == 0 = create el
            | check nel = rewind nel
            | _ = advance nel
            where nel = el - 1
        advance state el = create state $ el + 1
        create state el = if el == (length els - 1) then newState else advance newState el
            where 
                newHTMLElement = insertHTMLElement els!!el
                newState = set activeFormattingElements (insertAt newHTMLElement el (_activeFormattingElements state))

clearActiveFormatting :: State -> State
clearActiveFormatting state = set activeFormattingElements (clearEl (_activeFormattingElements state)) state
    where clearEl (el:els)
            | el == Marker = clearEl els
            | _ = els

doSelect :: [Tag] -> InsertionMode
doSelect [] = Initial
doSelect [_] = InSelect
doSelect (el:els) =
    case tracer $ tagType el of
        Template -> InSelect
        Table -> InSelectInTable
        _ -> doSelect els

resetInsertionMode :: State -> State
resetInsertionMode state = _resetInsertionMode 0 state

_resetInsertionMode :: Int -> State -> State
_resetInsertionMode idx state =
    let
        opened = _openElements state
        isLast = idx == (length opened) - 1
    in case (isLast, tagType (opened!!idx)) of
        (_, Select) -> set mode (doSelect opened) state
        (False, Td) -> set mode InCell state
        (False, Th) -> set mode InCell state
        (_, Tr) -> set mode InRow state
        (_, TBody) -> set mode InTableBody state
        (_, THead) -> set mode InTableBody state
        (_, TFoot) -> set mode InTableBody state
        (_, Caption) -> set mode InCaption state
        (_, Colgroup) -> set mode InColgroup state
        (_, Table) -> set mode InTable state
        (_, Template) -> set mode (head $ view templateModes state) state
        (_, Head) -> set mode InHead state
        (_, Body) -> set mode InBody state
        (_, FrameSet) -> set mode InFrameSet state
        (_, Html) -> set mode (case view headPointer state of
                Nothing -> BeforeHead
                (Just _) -> AfterHead)
                state
        (True, _) -> set mode InBody state
        _ -> _resetInsertionMode (idx + 1) state

parseString :: String -> (String, Either Error String)
parseString str = ("", Right "")
