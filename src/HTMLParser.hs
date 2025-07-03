{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wall #-}

module HTMLParser where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Debug.Trace (trace)
import Control.Monad.Fix (fix)

import Lens.Micro.Mtl (view, zoom)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)
import Data.Char (toUpper, toLower)
import qualified Data.Set as S
import Data.Either (isRight, fromRight)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, listToMaybe)
import Data.Map (keys, (!))
import Data.Char (chr)
import Data.List (sortOn, elemIndex)

import qualified CharacterReferences as C

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

data TagType = 
    Applet |
    Object |
    Marquee |
    Template {
        templateContents :: Node
    } |
    Td |
    Th |
    Caption |
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

data CustomElementDefinition = CustomElementDefinition {
    name :: String
    , localName :: String
} deriving (Show, Eq)
data CustomElementRegistry = CustomElementRegistry {
    customElementDefinitionSet :: [CustomElementDefinition]
} deriving (Show, Eq)

lookUpACustomElementDefinition :: Nodeable a => Maybe CustomElementRegistry -> Maybe String -> String -> Maybe String -> Maybe a
lookUpACustomElementDefinition registry p_nameSpace p_localName is =
    if isNothing registry || fromMaybe "" p_nameSpace /= "html"
    then Nothing
    else listToMaybe $ filter (\ el -> localName el == p_localName && (name el == p_localName || Just (name el) == is)) (customElementDefinitionSet $ fromJust registry)


class Nodeable a where
    getNode :: a -> Node
    getCustomElementRegistry :: a -> Maybe CustomElementRegistry

data Document = Document {
    _documentNode :: Node
    , _documentCustomElementRegistry :: CustomElementRegistry
} deriving (Show, Eq)
instance Nodeable Document where
    getNode this = _documentNode this
    getCustomElementRegistry this = Just $ _documentCustomElementRegistry this

data Element = Element {
    _elementNode :: Node
    , _elementCustomElementRegistry :: CustomElementRegistry
} deriving (Show, Eq)
instance Nodeable Element where
    getNode this = _elementNode this
    getCustomElementRegistry this = Just $ _elementCustomElementRegistry this

data ShadowRoot = ShadowRoot {
    _shadowRootNode :: Node
    , _shadowRootCustomElementRegistry :: CustomElementRegistry
} deriving (Show, Eq)
instance Nodeable ShadowRoot where
    getNode this = _shadowRootNode this
    getCustomElementRegistry this = Just $ _shadowRootCustomElementRegistry this

data Node = Node {
    _nodeName :: String
    , _nodeTagType :: TagType
    , _nodeAttrs :: [Attribute]
    , _nodeNameSpace :: String
    , _nodeIsStart :: Bool
    , _nextNodes :: [Node]
    , _parent :: Maybe Node
    , _nodeDocument :: Document
} deriving (Show, Eq)
$(makeLenses ''Node)
instance Nodeable Node where
    getNode this = this
    getCustomElementRegistry _ = Nothing

data Position = Position (Node, Int) deriving Show

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
        Menu,
        Meta,
        Nav,
        Noembed,
        Noframes,
        Noscript,
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
        Textarea,
        Tfoot,
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
    _tagType :: TagType
    , _tagName :: String
    , _nameSpace :: String
    , _attrs :: [Attribute]
    , _selfClosing :: Bool
    , _opening :: Bool
} deriving (Show, Eq)
$(makeLenses ''Tag)

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
  (Template _) -> TheTemplate
  Script -> RawText
  Style -> RawText
  TextArea -> EscapableText
  Title -> EscapableText
  SVG -> Foreign
  _ -> Normal

strToType :: String -> TagType
strToType str = case map toLower str of
  "select" -> Select
  "template" -> Template {}
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

makeTag :: Bool -> Tag
makeTag start = Tag {_tagType=Div, _tagName="", _nameSpace="", _attrs=[], _selfClosing=False, _opening=start}

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

alphaLower = satisfy (`elem` "abcdefghijklmnopqrstuvwxyz")
alphaUpper = satisfy (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
alpha = alphaLower <||> alphaUpper
digit = satisfy (`elem` "0123456789")
alphanumeric = alpha <||> digit
hexDigit = satisfy (`elem` "abcdefABCDEF") <||> digit

isControl n = 0x007f <= n && n <= 0x009f
isSurrogate n = (0xd800 <= n && n <= 0xdbff) || (0xdc00 <= n && n <= 0xdfff)
isWhitespace n = 0x007f <= n && n <= 0x009f

parseTagName = manyParser alphanumeric

legacy :: Parser String
legacy = doSequentialString [somespace, matchStringIgnoreCase "SYSTEM", somespace, (matchStringIgnoreCase "'about:legacy-compat'" <||> matchStringIgnoreCase "\"about:legacy-compat\"")]

doctype :: Parser String
doctype = doSequentialString [matchStringIgnoreCase "<!DOCTYPE", somespace, matchStringIgnoreCase "html", whoCares legacy $ manyspace, matchString ">"]

preProcess :: String -> String
preProcess str = filter (/= '\r') str

data InsertionMode = Initial | InSelect | InSelectInTable | InCell | InRow | InTableBody | InCaption | InColgroup | InTable | InHead | InBody | InFrameSet | BeforeHead | AfterHead | TestMode deriving (Show, Eq)

data StateMachineState = 
    DataState 
    | CharacterReferenceState 
    | TagOpenState 
    | RCDataState 
    | RawTextState 
    | RCDataLessThanSignState 
    | ScriptDataState 
    | ScriptDataLessThanSignState 
    | PlainTextState 
    | MarkupDeclarationOpenState 
    | EndTagOpenState 
    | RawTextLessThanSignState 
    | TagNameState 
    | BogusCommentState 
    | BeforeAttributeNameState 
    | SelfClosingStartTag 
    | RCDataEndTagOpenState 
    | RCDataEndTagNameState 
    | RawTextEndTagOpenState 
    | RawTextEndTagNameState 
    | ScriptDataEndTagOpenState 
    | ScriptDataEscapeStartState 
    | ScriptDataEndTagNameState 
    | ScriptDataEscapeStartDashState 
    | ScriptDataEscapedDashDashState 
    | ScriptDataEscapedState 
    | ScriptDataEscapedDashState 
    | ScriptDataEscapedLessThanSignState 
    | ScriptDataEscapedEndTagOpenState 
    | ScriptDataDoubleEscapeStartState 
    | ScriptDataEscapedEndTagNameState 
    | ScriptDataDoubleEscapedState 
    | ScriptDataDoubleEscapedLessThanSignState 
    | ScriptDataDoubleEscapedDashState 
    | ScriptDataDoubleEscapedDashDashState 
    | ScriptDataDoubleEscapeEndState 
    | AfterAttributeNameState 
    | AttributeNameState 
    | BeforeAttributeValueState 
    | AttributeValueDoubleQuotedState 
    | AttributeValueSingleQuotedState 
    | AttributeValueUnquotedState 
    | AfterAttributeValueQuotedState 
    | DOCTYPEState 
    | CommentStartState 
    | CDATASectionState 
    | CommentStartDashState 
    | CommentState 
    | CommentEndState 
    | CommentEndDashState 
    | CommentLessThanSignState 
    | CommentLessThanSignBangState 
    | CommentLessThanSignBangDashState 
    | CommentLessThanSignBangDashDashState 
    | CommentEndBangState 
    | BeforeDOCTYPENameState 
    | DOCTYPENameState 
    | AfterDOCTYPENameState 
    | AfterDOCTYPEPublicKeywordState 
    | AfterDOCTYPESystemKeywordState 
    | BogusDOCTYPEState 
    | BeforeDOCTYPEPublicIdentifierState 
    | DOCTYPEPublicIdentifierDoubleQuotedState 
    | DOCTYPEPublicIdentifierSingleQuotedState 
    | AfterDOCTYPEPublicIdentifierState 
    | BetweenDOCTYPEPublicAndSystemIdentifiersState 
    | DOCTYPESystemIdentifierDoubleQuotedState 
    | DOCTYPESystemIdentifierSingleQuotedState 
    | BeforeDOCTYPESystemIdentifierState 
    | AfterDOCTYPESystemIdentifierState 
    | CDATASectionBracketState 
    | CDATASectionEndState 
    | NamedCharacterReferenceState 
    | NumericCharacterReferenceState 
    | AmbiguousAmpersandState 
    | DecimalCharacterReferenceStartState 
    | HexadecimalCharacterReferenceState 
    | HexadecimalCharacterReferenceStartState 
    | DecimalCharacterReferenceState 
    | NumericCharacterReferenceEndState
    deriving (Show, Eq)

type ActiveFormattingElement = Maybe Node

data Comment = Comment String deriving (Show, Eq)

data DOCTYPE = DOCTYPE {
    _enableQuirksFlag :: Bool
    , _name :: Maybe String
    , _system :: Maybe String
    , _public :: Maybe String
} deriving (Show, Eq)
$(makeLenses ''DOCTYPE)

data Token = 
    Character Char
    | TagToken Tag 
    | CommentToken Comment
    | DOCTYPEToken DOCTYPE
    | EOF deriving (Show, Eq)

data State = State {
    _stateMachineState :: StateMachineState
    , _returnState :: StateMachineState
    , _input :: String
    , _openElements :: [Node]
    , _activeFormattingElements :: [ActiveFormattingElement]
    , _mode :: InsertionMode
    , _templateModes :: [InsertionMode]
    , _headPointer :: Maybe Tag 
    , _formPointer :: Maybe Tag
    , _scriptingEnabled :: Bool
    , _framesetOk :: Bool
    , _currentTagToken :: Tag
    , _currentCommentToken :: Comment
    , _currentDOCTYPEToken :: DOCTYPE
    , _temporaryBuffer :: String
    , _characterReferenceCode :: Int
    , _lastEmitted :: Token
    , _lastStart :: Maybe Tag
} deriving (Show)
$(makeLenses ''State)

getNextInputCharacter :: State -> (State, Char)
getNextInputCharacter state =
    let out = (_input state)!!0
    in (over input (drop 1) state, out)

emitToken :: Token -> State -> State
emitToken (TagToken t) state = (if _opening t
    then set lastStart (Just t)
    else id) $ set lastEmitted (trace (show t) TagToken t) state
emitToken token state = trace (show token) $ set lastEmitted token state

doStateMachine :: State -> State
doStateMachine state = case _stateMachineState state of
    DataState -> case nextInputCharacter of
        '&' -> set returnState DataState (set stateMachineState CharacterReferenceState state')
        '<' -> set stateMachineState TagOpenState state'
        '\0' -> emitToken (Character nextInputCharacter) state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    RCDataState -> case nextInputCharacter of
        '&' -> set returnState RCDataState (set stateMachineState CharacterReferenceState state')
        '<' -> set stateMachineState RCDataLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    RawTextState -> case nextInputCharacter of
        '<' -> set stateMachineState RawTextLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    ScriptDataState -> case nextInputCharacter of
        '<' -> set stateMachineState ScriptDataLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    PlainTextState -> case nextInputCharacter of
        '\0' -> emitToken (Character '\xfffd') state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    TagOpenState
        | nextInputCharacter == '!' -> set stateMachineState MarkupDeclarationOpenState state'
        | nextInputCharacter == '/' -> set stateMachineState EndTagOpenState state' 
        | isRight $ snd $ parse alpha [nextInputCharacter] -> set stateMachineState TagNameState $ set currentTagToken (makeTag True) state
        | nextInputCharacter == '?' -> set stateMachineState BogusCommentState $ set currentCommentToken (Comment "") state
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (Character '<') state'
        | otherwise -> set stateMachineState DataState $ emitToken (Character '<') state
    EndTagOpenState
        | isRight $ snd $ parse alpha [nextInputCharacter] -> set stateMachineState TagNameState $ set currentTagToken (makeTag False) state
        | nextInputCharacter == '>' -> set stateMachineState DataState state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (Character '/') $ emitToken (Character '<') state'
        | otherwise -> set stateMachineState BogusCommentState $ set currentCommentToken (Comment "") state
    TagNameState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeAttributeNameState state'
        | nextInputCharacter == '/' -> set stateMachineState SelfClosingStartTag state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | followsParse alphaUpper -> appendCharacter nextInputCharacter
        | nextInputCharacter == '\0' -> appendCharacter '\xfffd'
        | nextInputCharacter == '#' -> emitToken EOF state'
        | otherwise -> appendCharacter nextInputCharacter
    RCDataLessThanSignState -> if nextInputCharacter == '/'
        then set stateMachineState RCDataEndTagOpenState (set temporaryBuffer "" state')
        else set stateMachineState RCDataState (emitToken (Character '<') state)
    RCDataEndTagOpenState -> if followsParse alpha
        then set stateMachineState RCDataEndTagNameState state
        else set stateMachineState RCDataState $ emitToken (Character '/') $ emitToken (Character '<') state
    RCDataEndTagNameState -> doEndNameState RCDataState
    RawTextLessThanSignState -> if nextInputCharacter == '/'
        then set stateMachineState RawTextEndTagOpenState (set temporaryBuffer "" state')
        else set stateMachineState RawTextState (emitToken (Character '<') state)
    RawTextEndTagOpenState -> if followsParse alpha
        then set stateMachineState RawTextEndTagNameState $ set currentTagToken (makeTag False) state
        else set stateMachineState RawTextState $ emitToken (Character '/') $ emitToken (Character '<') state
    RawTextEndTagNameState -> doEndNameState RawTextState
    ScriptDataLessThanSignState -> case nextInputCharacter of
        '/' -> set stateMachineState ScriptDataEndTagOpenState (set temporaryBuffer "" state')
        '!' -> emitToken (Character '!') $ emitToken (Character '<') $ set stateMachineState ScriptDataEscapeStartState state'
        _ -> set stateMachineState ScriptDataState (emitToken (Character '<') state)
    ScriptDataEndTagOpenState -> if followsParse alpha
        then set stateMachineState ScriptDataEndTagNameState state
        else set stateMachineState ScriptDataState $ emitToken (Character '/') $ emitToken (Character '<') state
    ScriptDataEndTagNameState -> doEndNameState ScriptDataState
    ScriptDataEscapeStartState -> if nextInputCharacter == '-'
        then emitToken (Character '-') $ set stateMachineState ScriptDataEscapeStartDashState state'
        else set stateMachineState ScriptDataState state
    ScriptDataEscapeStartDashState -> if nextInputCharacter == '-'
        then emitToken (Character '-') $ set stateMachineState ScriptDataEscapedDashDashState state'
        else set stateMachineState ScriptDataState state
    ScriptDataEscapedState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataEscapedDashState state'
        '<' -> set stateMachineState ScriptDataEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    ScriptDataEscapedDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataEscapedDashDashState state'
        '<' -> set stateMachineState ScriptDataEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataEscapedState state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataEscapedState state'
    ScriptDataEscapedDashDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') state'
        '<' -> set stateMachineState ScriptDataEscapedLessThanSignState state'
        '>' -> emitToken (Character '>') $ set stateMachineState ScriptDataState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataEscapedState state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataEscapedState state'
    ScriptDataEscapedLessThanSignState
        | nextInputCharacter == '/' -> set stateMachineState ScriptDataEscapedEndTagOpenState $ set temporaryBuffer "" state'
        | followsParse alpha -> set stateMachineState ScriptDataDoubleEscapeStartState $ emitToken (Character '<') $ set temporaryBuffer "" state
        | otherwise -> set stateMachineState ScriptDataEscapedState $ emitToken (Character '<') state
    ScriptDataEscapedEndTagOpenState -> if followsParse alpha
        then set stateMachineState ScriptDataEscapedEndTagNameState $ set currentTagToken (makeTag False) state
        else set stateMachineState ScriptDataEscapedState $ emitToken (Character '/') $ emitToken (Character '<') state
    ScriptDataEscapedEndTagNameState -> doEndNameState ScriptDataEscapedState
    ScriptDataDoubleEscapeStartState
        | nextInputCharacter `elem` "\t\n\f />" -> emitToken (Character nextInputCharacter) $ set stateMachineState (if _temporaryBuffer state' == "script"
            then ScriptDataDoubleEscapedState
            else ScriptDataEscapedState) state'
        | followsParse alphaUpper -> emitToken (Character nextInputCharacter) $ over temporaryBuffer (++[toLower nextInputCharacter]) state'
        | otherwise -> set stateMachineState ScriptDataEscapedState state
    ScriptDataDoubleEscapedState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataDoubleEscapedDashState state'
        '<' -> emitToken (Character '<') $ set stateMachineState ScriptDataDoubleEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    ScriptDataDoubleEscapedDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataDoubleEscapedDashDashState state'
        '<' -> set stateMachineState ScriptDataDoubleEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataDoubleEscapedState state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataDoubleEscapedState state'
    ScriptDataDoubleEscapedDashDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') state'
        '<' -> emitToken (Character '<') $ set stateMachineState ScriptDataDoubleEscapedLessThanSignState state'
        '>' -> emitToken (Character '>') $ set stateMachineState ScriptDataState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataDoubleEscapedState state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataDoubleEscapedState state'
    ScriptDataDoubleEscapedLessThanSignState -> if nextInputCharacter == '/'
        then emitToken (Character '/') $ set stateMachineState ScriptDataDoubleEscapeEndState $ set temporaryBuffer "" state'
        else set stateMachineState ScriptDataDoubleEscapedState state
    ScriptDataDoubleEscapeEndState
        | nextInputCharacter `elem` "\t\n\f />" -> emitToken (Character nextInputCharacter) $ set stateMachineState (if _temporaryBuffer state' == "script"
            then ScriptDataEscapedState
            else ScriptDataDoubleEscapedState) state'
        | followsParse alphaUpper -> emitToken (Character nextInputCharacter) $ over temporaryBuffer (++[toLower nextInputCharacter]) state'
        | otherwise -> set stateMachineState ScriptDataDoubleEscapedState state
    BeforeAttributeNameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter `elem` "/>#" -> set stateMachineState AfterAttributeNameState state
        | nextInputCharacter == '=' -> set stateMachineState AttributeNameState $ startAttribute state'
        | otherwise -> set stateMachineState AttributeNameState $ startAttribute state
    AttributeNameState -- if duplicate, should remove the new one
        | nextInputCharacter `elem` "\t\n\f />#" -> set stateMachineState AfterAttributeNameState state
        | nextInputCharacter == '=' -> set stateMachineState BeforeAttributeValueState state'
        | followsParse alphaUpper -> appendToAttrName (toLower nextInputCharacter) state'
        | nextInputCharacter == '\0' -> appendToAttrName '\xfffd' state'
        | otherwise -> appendToAttrName nextInputCharacter state' 
    AfterAttributeNameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '/' -> set stateMachineState SelfClosingStartTag state'
        | nextInputCharacter == '=' -> set stateMachineState BeforeAttributeValueState state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '#' -> emitToken EOF state'
        | otherwise -> set stateMachineState AttributeNameState $ startAttribute state'
    BeforeAttributeValueState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '"' -> set stateMachineState AttributeValueDoubleQuotedState state'
        | nextInputCharacter == '\'' -> set stateMachineState AttributeValueSingleQuotedState state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | otherwise -> set stateMachineState AttributeValueUnquotedState state
    AttributeValueDoubleQuotedState -> case nextInputCharacter of
        '"' -> set stateMachineState AfterAttributeValueQuotedState state'
        '&' -> set stateMachineState CharacterReferenceState $ set returnState AttributeValueDoubleQuotedState state'
        '\0' -> appendToAttr '\xfffd' state'
        '#' -> emitToken EOF state'
        _ -> appendToAttr nextInputCharacter state'
    AttributeValueSingleQuotedState -> case nextInputCharacter of
        '\'' -> set stateMachineState AfterAttributeValueQuotedState state'
        '&' -> set stateMachineState CharacterReferenceState $ set returnState AttributeValueSingleQuotedState state'
        '\0' -> appendToAttr '\xfffd' state'
        '#' -> emitToken EOF state'
        _ -> appendToAttr nextInputCharacter state'
    AttributeValueUnquotedState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeAttributeNameState state'
        | nextInputCharacter == '&' -> set stateMachineState CharacterReferenceState $ set returnState AttributeValueUnquotedState state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\0' -> emitToken (Character '\xfffd') state'
        | nextInputCharacter == '#' -> emitToken EOF state'
        | otherwise -> appendToAttr nextInputCharacter state'
    AfterAttributeValueQuotedState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeAttributeNameState state'
        | nextInputCharacter == '/' -> set stateMachineState SelfClosingStartTag state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '#' -> emitToken EOF state'
        | otherwise -> set stateMachineState BeforeAttributeNameState state
    SelfClosingStartTag -> case nextInputCharacter of
        '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState $ over currentTagToken (set selfClosing True) state'
        '#' -> emitToken EOF state'
        _ -> set stateMachineState BeforeAttributeNameState state
    BogusCommentState -> case nextInputCharacter of
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '#' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        '\0' -> appendToComment '\xfffd' state'
        _ -> appendToComment nextInputCharacter state'
    MarkupDeclarationOpenState
        | isRight outComment -> set stateMachineState CommentStartState $ set input restComment $ set currentCommentToken (Comment "") state
        | isRight outDoctype -> set stateMachineState DOCTYPEState $ set input restDoctype state
        | isRight outCData -> (if length (_openElements state') /= 0 && not (inHtmlNamespace ((_openElements state')!!0))
            then set stateMachineState CDATASectionState
            else set stateMachineState BogusCommentState) $ set input restCData $ set currentCommentToken (Comment "") state
        | otherwise -> set stateMachineState BogusCommentState $ set currentCommentToken (Comment "") state
        where
            inHtmlNamespace tag = True
            (restComment, outComment) = parse (matchString "--") $ _input state
            (restDoctype, outDoctype) = parse (matchStringIgnoreCase "DOCTYPE") $ _input state
            (restCData, outCData) = parse (matchString "[CDATA[") $ _input state
    CommentStartState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentStartDashState state'
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        _ -> set stateMachineState CommentState state
    CommentStartDashState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentEndState state'
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '#' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '-' state'
    CommentState -> case nextInputCharacter of
        '<' -> set stateMachineState CommentLessThanSignState $ appendToComment nextInputCharacter state'
        '-' -> set stateMachineState CommentEndDashState state'
        '\0' -> appendToComment '\xfffd' state'
        '#' -> emitToken EOF $ emitToken (Character nextInputCharacter) state'
        _ -> appendToComment nextInputCharacter state'
    CommentLessThanSignState -> case nextInputCharacter of
        '!' -> set stateMachineState CommentLessThanSignBangState $ appendToComment nextInputCharacter state'
        '<' -> appendToComment nextInputCharacter state'
        _ -> set stateMachineState CommentState state
    CommentLessThanSignBangState -> if nextInputCharacter == '-'
        then set stateMachineState CommentLessThanSignBangDashState state'
        else set stateMachineState CommentState state
    CommentLessThanSignBangDashState -> if nextInputCharacter == '-'
        then set stateMachineState CommentLessThanSignBangDashDashState state'
        else set stateMachineState CommentEndDashState state
    CommentLessThanSignBangDashDashState -> set stateMachineState CommentEndState state
    CommentEndDashState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentEndState state'
        '#' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '-' state
    CommentEndState -> case nextInputCharacter of
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '!' -> set stateMachineState CommentEndBangState state'
        '-' -> appendToComment '-' state'
        '#' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '-' $ appendToComment '-' state
    CommentEndBangState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentEndDashState $ appendToComment '!' $ appendToComment '-' $ appendToComment '-' state'
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '#' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '!' $ appendToComment '-' $ appendToComment '-' $ state
    DOCTYPEState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeDOCTYPENameState state'
        | nextInputCharacter == '>' -> set stateMachineState BeforeDOCTYPENameState state
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag $ createDoctype state'
        | otherwise -> set stateMachineState BeforeDOCTYPENameState state
    BeforeDOCTYPENameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '\0' -> set stateMachineState DOCTYPENameState $ ahhhh '\xfffd' $ createDoctype state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag $ createDoctype state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag $ createDoctype state'
        | otherwise -> set stateMachineState DOCTYPENameState $ ahhhh (toLower nextInputCharacter) $ createDoctype state'
        where ahhhh c = over currentDOCTYPEToken (set name $ Just [c])
    DOCTYPENameState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState AfterDOCTYPENameState state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\0' -> appendToDOCTYPE '\xfffd' name state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag $ state'
        | otherwise -> appendToDOCTYPE (toLower nextInputCharacter) name state'
    AfterDOCTYPENameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | isRight outPublic -> set stateMachineState AfterDOCTYPEPublicKeywordState $ set input restPublic state
        | isRight outSystem -> set stateMachineState AfterDOCTYPESystemKeywordState $ set input restSystem state
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
        where
            (restPublic, outPublic) = parse (matchStringIgnoreCase "public") $ _input state
            (restSystem, outSystem) = parse (matchStringIgnoreCase "system") $ _input state
    AfterDOCTYPEPublicKeywordState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeDOCTYPEPublicIdentifierState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPEPublicIdentifierDoubleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPEPublicIdentifierSingleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
    BeforeDOCTYPEPublicIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPEPublicIdentifierDoubleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPEPublicIdentifierSingleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
    DOCTYPEPublicIdentifierDoubleQuotedState -> case nextInputCharacter of
        '"' -> set stateMachineState AfterDOCTYPEPublicIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' public state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter public state'
    DOCTYPEPublicIdentifierSingleQuotedState -> case nextInputCharacter of
        '\'' -> set stateMachineState AfterDOCTYPEPublicIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' public state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter public state'
    AfterDOCTYPEPublicIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BetweenDOCTYPEPublicAndSystemIdentifiersState state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
    BetweenDOCTYPEPublicAndSystemIdentifiersState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
    AfterDOCTYPESystemKeywordState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeDOCTYPESystemIdentifierState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state'
    BeforeDOCTYPESystemIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
    DOCTYPESystemIdentifierDoubleQuotedState -> case nextInputCharacter of
        '"' -> set stateMachineState AfterDOCTYPESystemIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' system state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter system state'
    DOCTYPESystemIdentifierSingleQuotedState -> case nextInputCharacter of
        '\'' -> set stateMachineState AfterDOCTYPESystemIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' system state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter system state'
    AfterDOCTYPESystemIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState state
    BogusDOCTYPEState -> case nextInputCharacter of
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        '#' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') state'
        _ -> state'
    CDATASectionState -> case nextInputCharacter of
        ']' -> set stateMachineState CDATASectionBracketState state'
        '#' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'
    CDATASectionBracketState -> if nextInputCharacter == ']'
        then set stateMachineState CDATASectionEndState state'
        else set stateMachineState CDATASectionState $ emitToken (Character ']') state
    CDATASectionEndState -> case nextInputCharacter of
        ']' -> emitToken (Character ']') state'
        '>' -> set stateMachineState DataState state'
        _ -> set stateMachineState CDATASectionState $ emitToken (Character ']') $ emitToken (Character ']') state
    CharacterReferenceState
        | followsParse alphanumeric -> set stateMachineState NamedCharacterReferenceState $ setBuf state
        | nextInputCharacter == '#' -> set stateMachineState NumericCharacterReferenceState $ appendToTemporaryBuffer [nextInputCharacter] $ setBuf state'
        | otherwise -> set stateMachineState (_returnState state') $ flushCodePoints $ setBuf state'
        where setBuf s = set temporaryBuffer "&" s
    NamedCharacterReferenceState -> if length checked /= 0
        then if partOfAnAttribute state' && not semicoloned && (nextInputCharacter == '=' || followsParse alphanumeric)
            then set stateMachineState (_returnState state') $ flushCodePoints state'
            else set stateMachineState (_returnState state) $ flushCodePoints $ set temporaryBuffer [(C.characterReferences ! om)] $ set input rester state
        else set stateMachineState AmbiguousAmpersandState $ flushCodePoints state'
        where 
            checked = filter (\ key -> key == take (length key) (_input state)) $ keys C.characterReferences
            om = head $ reverse $ sortOn length checked
            rest = drop (length om) (_input state)
            semicoloned = head rest == ';'
            rester = drop (if semicoloned then 1 else 0) rest
    AmbiguousAmpersandState -> if followsParse alphanumeric 
        then if partOfAnAttribute state'
            then appendToAttr nextInputCharacter state'
            else emitToken (Character nextInputCharacter) state'
        else set stateMachineState (_returnState state) state
    NumericCharacterReferenceState -> if toLower nextInputCharacter == 'x'
        then set stateMachineState HexadecimalCharacterReferenceStartState $ appendToTemporaryBuffer [nextInputCharacter] $ getBuf state'
        else set stateMachineState DecimalCharacterReferenceStartState $ getBuf state
        where getBuf s = set characterReferenceCode 0 s
    HexadecimalCharacterReferenceStartState -> if followsParse hexDigit
        then set stateMachineState HexadecimalCharacterReferenceState state
        else set stateMachineState (_returnState state') $ flushCodePoints state
    DecimalCharacterReferenceStartState -> if followsParse digit
        then set stateMachineState DecimalCharacterReferenceState state
        else set stateMachineState (_returnState state') $ flushCodePoints state
    HexadecimalCharacterReferenceState
        | followsParse hexDigit -> over characterReferenceCode (\ n -> n * 16 + (hexToInt nextInputCharacter)) state'
        | nextInputCharacter == ';' -> set stateMachineState NumericCharacterReferenceEndState state'
        | otherwise -> set stateMachineState NumericCharacterReferenceEndState state
    DecimalCharacterReferenceState
        | followsParse hexDigit -> over characterReferenceCode (\ n -> n * 10 + (read [nextInputCharacter])) state'
        | nextInputCharacter == ';' -> set stateMachineState NumericCharacterReferenceEndState state'
        | otherwise -> set stateMachineState NumericCharacterReferenceEndState state
    NumericCharacterReferenceEndState
        | code == 0 || code > 0x10ffff || isSurrogate code -> set characterReferenceCode 0xfffd state
        | code == 0x0d || (isControl code && (not $ isWhitespace code)) -> set characterReferenceCode (C.errToCodePoint ! fromIntegral code) state
        | otherwise -> set stateMachineState (_returnState state) $ flushCodePoints $ set temporaryBuffer [chr code] state
        where code = _characterReferenceCode state
    where
        startAttribute s = over currentTagToken (\ tag -> over attrs (Attribute ("", ""):) tag) s
        partOfAnAttribute s = (_returnState s) `elem` [AttributeValueDoubleQuotedState, AttributeValueSingleQuotedState, AttributeValueUnquotedState]
        flushCodePoints s = set temporaryBuffer "" (if partOfAnAttribute s
            then s
            else foldr emitToken s (map Character $ _temporaryBuffer s))
        appendToTemporaryBuffer c s = over temporaryBuffer (++c) s
        createDoctype s = set currentDOCTYPEToken DOCTYPE {_enableQuirksFlag=False, _name=Nothing, _system=Nothing, _public=Nothing} s
        setDOCTYPEFlag = over currentDOCTYPEToken (set enableQuirksFlag True)
        setDOCTYPEInitial setter = over currentDOCTYPEToken (set setter $ Just "")
        appendToDOCTYPE toAppend setter = over currentDOCTYPEToken (over setter doAppend)
            where 
                doAppend (Just str) = Just $ str ++ [toAppend]
                doAppend Nothing = Just [toAppend]
        appendToAttrName c s = over currentTagToken (over attrs $ doThing c) s
            where
                doThing toAppend a = doThinger toAppend (head a) : drop 1 a
                doThinger toAppend (Attribute attr) = Attribute (fst attr ++ [toAppend], snd attr)
        appendToAttr c s = over currentTagToken (over attrs $ doThing c) s
            where
                doThing toAppend a = doThinger toAppend (head a) : drop 1 a
                doThinger toAppend (Attribute attr) = Attribute (fst attr, snd attr ++ [toAppend])
        appendToComment c s = over currentCommentToken (\ (Comment thing) -> Comment $ thing ++ [c]) s
        (state', nextInputCharacter) = getNextInputCharacter state
        followsParse a = isRight $ snd $ parse a [nextInputCharacter]
        appropriateEndTagToken = case _lastStart state of
            Just t -> _tagName t == _tagName (_currentTagToken state)
            Nothing -> False
        appendCharacter c = over currentTagToken (over tagName (++[c])) state'
        doEndNameState endState
            | appropriateEndTagToken && nextInputCharacter `elem` "\t\n\f " = set stateMachineState BeforeAttributeNameState state'
            | appropriateEndTagToken && nextInputCharacter == '/' = set stateMachineState SelfClosingStartTag state'
            | appropriateEndTagToken && nextInputCharacter == '>' = emitToken (TagToken $ _currentTagToken state') (set stateMachineState DataState state')
            | followsParse alpha = over temporaryBuffer (++[toLower nextInputCharacter]) $ appendCharacter nextInputCharacter
            | otherwise = set stateMachineState endState (foldr (\ a b -> emitToken (Character a) b) (emitToken (Character '/') (emitToken (Character '<') state)) (_temporaryBuffer state))
        hexToInt c = read $ "0x" ++ [c]

getAttr :: String -> Node -> Maybe Attribute
getAttr attrName tag = if length check /= 0
    then Just $ head check
    else Nothing
    where check = filter (\ (Attribute (n, _)) -> n == attrName) $ _nodeAttrs tag

tagGetAttr :: String -> Tag -> Maybe Attribute
tagGetAttr attrName tag = if length check /= 0
    then Just $ head check
    else Nothing
    where check = filter (\ (Attribute (n, _)) -> n == attrName) $ _attrs tag


inHTMLNamespace :: Node -> Bool
inHTMLNamespace token =  _nodeNameSpace token == "http://www.w3.org/1999/xhtml"

isMathMLTextIntegrationPoint :: Node -> Bool
isMathMLTextIntegrationPoint token = _nodeTagType token `elem` [MathMLMi, MathMLMo, MathMLMn, MathMLMs, MathMLMtext]

isHTMLIntegrationPoint :: Node -> Bool
isHTMLIntegrationPoint token =
    _nodeTagType token `elem` [SVGForeignObject, SVGDesc, SVGTitle] 
    || (_nodeTagType token == MathMLAnnotationXml
        && (case at of
            (Just (Attribute (_, val))) -> val `elem` ["text/html", "application/xhtml+xml"]
            _ -> False))
        where at = getAttr "encoding" token

treeConstructionDispatch :: Token -> State -> State
treeConstructionDispatch token state = if 
    length (_openElements state) == 0
    || inHTMLNamespace currentNode 
    || isMathMLTextIntegrationPoint currentNode && ((_nodeIsStart currentNode && (not $ _nodeName currentNode `elem` ["mglyph", "malignmark"])) || isCharacter)
    || _nodeIsStart currentNode && _nodeTagType currentNode == MathMLAnnotationXml && _nodeName currentNode == "svg"
    || (isHTMLIntegrationPoint currentNode && (_nodeIsStart currentNode || isCharacter))
    || token == EOF
    then parseByInsertionMode state
    else parseForeign state
    where 
        currentNode = head $ _openElements state
        isCharacter = case token of
            (Character _) -> True
            _ -> False

parseByInsertionMode :: State -> State
parseByInsertionMode state = state

parseForeign :: State -> State
parseForeign state = state

findAppropriatePlaceForInsertingNode :: Bool -> Maybe Node -> State -> Position
findAppropriatePlaceForInsertingNode fosterParenting override state = 
    let
        adjustedInsertionLocation = if fosterParenting && _nodeTagType target `elem` [Table, TBody, TFoot, Tr]
            then if
                | isJust lastTemplate && templateIndex > fromJust (elemIndex lable (_openElements state)) -> let temple = fromJust lastTemplate in Position (temple, length (_nextNodes temple) - 1)
                | hasParent lable -> 
                    let tableParent = fromJust $ _parent lable
                    in Position (tableParent, fromJust $ elemIndex lable $ _nextNodes tableParent)
                | otherwise -> 
                    let 
                        yindex = elemIndex lable (_openElements state)
                        previousElement = (_openElements state) !! (fromJust yindex - 1)
                    in Position (previousElement, length (_nextNodes previousElement) - 1)
            else Position (target, length (_nextNodes target) - 1)
        (Position (el, _)) = adjustedInsertionLocation 
    in
        case _nodeTagType el of
            (Template temple) -> let conts = templateContents $ Template temple in Position (conts, length (_nextNodes conts) - 1)
            _ -> adjustedInsertionLocation
    where
        lable = fromJust lastTable
        hasParent el = isJust $ _parent el
        lastTemplate = lastElementOfTemplate $ _openElements state
        lastTable = lastElementOfType Table $ _openElements state
        templateIndex = fromJust $ elemIndex (fromJust lastTemplate) (_openElements state)
        isTemplate el = case _nodeTagType el of
            (Template _) -> True
            _ -> False
        lastElementOfTemplate [] = Nothing
        lastElementOfTemplate (el:els) = if isTemplate el
            then Just el
            else lastElementOfTemplate els
        lastElementOfType _ [] = Nothing
        lastElementOfType toFind (el:els) = if _nodeTagType el == toFind
            then Just el
            else lastElementOfType toFind els
        target = case override of
            (Just o) -> o
            _ -> head $ _openElements state

createAnElement :: Document -> String -> Maybe String -> Maybe String -> Maybe String -> Bool -> CustomElementRegistry
createAnElement document localName p_nameSpace prefix is synchronousCustomElements registry = Node {}

createAnElementForAToken :: Nodeable a => Token -> String -> a -> Node
createAnElementForAToken token p_nameSpace intendedParent =
    let
        document = intendedParent
        localName = _tagName tagoken
        is = tagGetAttr "is" tagoken
        registry = getCustomElementRegistry intendedParent
        definition = lookUpACustomElementDefinition registry p_nameSpace localName is
        willExecuteScript = isJust definition

        document' = if willExecuteScript
            then over (+1) throwOnDynamicMarkupInsertionCounter document
            else document
        element = createAnElement document' localName p_nameSpace Nothing is willExecuteScript registry
        element' = over (++ _attrs token) nodeAttrs element 
    in
        element'
    where 
        tagoken = case token of
            (TagToken t) -> t
            _ -> Tag {}

lastMarker :: State -> [Node]
lastMarker state = cutUntilMarker [] $ reverse $ _activeFormattingElements state
    where
        cutUntilMarker out [] = out
        cutUntilMarker out (Nothing:_) = out
        cutUntilMarker out (Just el:els) = cutUntilMarker (el:out) els


pushToActiveFormatting :: State -> Node -> State
pushToActiveFormatting state tag =
    let
        elements = _activeFormattingElements state
        countTags = foldl (\ a b -> a + if S.fromList (_nodeAttrs b) == S.fromList (_nodeAttrs tag) then 1 else 0) 0 (lastMarker state)
        elements' = 
            if countTags >= 3 then drop 1 elements
            else elements
    in
        set activeFormattingElements ((Just tag):elements') state

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

reconstructActiveFormatting :: State -> State
reconstructActiveFormatting state
    | length els == 0 = state
    | isNothing $ els!!0 = state
    | els!!0 `elem` (map (\ e -> Just e) (_openElements state)) = state
    | otherwise = rewind 0
    where 
        els = (_activeFormattingElements state)
        check el = isJust (els!!el) && not ((fromJust $ els!!el) `elem` _openElements state)
        rewind el
            | el == 0 = create state el
            | check nel = rewind nel
            | otherwise = advance state nel
            where nel = el - 1
        advance state el = create state $ el + 1

        create state el = if el == (length els - 1) then newState else advance newState el
            where 
                insertHTMLElement el = Just Node {}
                newHTMLElement = insertHTMLElement (els!!el)
                newState = set activeFormattingElements (insertAt newHTMLElement el (_activeFormattingElements state)) state

clearActiveFormatting :: State -> State
clearActiveFormatting state = set activeFormattingElements (clearEl (_activeFormattingElements state)) state
    where 
        clearEl [] = []
        clearEl (el:els)
            | isNothing el = clearEl els
            | otherwise = els

doSelect :: [Node] -> InsertionMode
doSelect [] = Initial
doSelect [_] = InSelect
doSelect (el:els) =
    case _nodeTagType el of
        (Template _) -> InSelect
        Table -> InSelectInTable
        _ -> doSelect els

resetInsertionMode :: State -> State
resetInsertionMode state = _resetInsertionMode 0 state

_resetInsertionMode :: Int -> State -> State
_resetInsertionMode idx state =
    let
        opened = _openElements state
        isLast = idx == (length opened) - 1
    in case (isLast, _nodeTagType (opened!!idx)) of
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
        (_, (Template _)) -> set mode (head $ view templateModes state) state
        (_, Head) -> set mode InHead state
        (_, Body) -> set mode InBody state
        (_, FrameSet) -> set mode InFrameSet state
        (_, Html) -> set mode (case view headPointer state of
                Nothing -> BeforeHead
                (Just _) -> AfterHead)
                state
        (True, _) -> set mode InBody state
        _ -> _resetInsertionMode (idx + 1) state

parseString :: String -> State
parseString str = 
    _parseString State {
        _stateMachineState = DataState
        , _returnState = DataState
        , _input = preProcess str ++ "#"
        , _openElements = []
        , _activeFormattingElements = []
        , _mode = Initial
        , _templateModes = []
        , _headPointer = Nothing
        , _formPointer = Nothing
        , _scriptingEnabled = True
        , _framesetOk = True
        , _currentTagToken = Tag {}
        , _currentCommentToken = Comment ""
        , _currentDOCTYPEToken = DOCTYPE {}
        , _temporaryBuffer = ""
        , _characterReferenceCode = 0
        , _lastEmitted = (Character 'a')
        , _lastStart = Nothing
    }

_parseString :: State -> State
_parseString state = if _lastEmitted state /= EOF
    then _parseString $ trace (show $ _stateMachineState state) $ doStateMachine state
    else state
