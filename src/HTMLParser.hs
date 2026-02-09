{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module HTMLParser where

import Debug.Trace (trace)

import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, over)
import Data.Char (toLower, chr)
import Data.Map (keys, (!))
import Data.List (sortOn)

import CharacterReferences

tracer :: Show a => a -> a
tracer a = trace (show a) a

newtype Attribute = Attribute (String, String) deriving (Show, Eq, Ord)

data Tag = Tag {
    _tagName :: String
    , _attrs :: [Attribute]
    , _selfClosing :: Bool
    , _opening :: Bool
} deriving (Show, Eq)
$(makeLenses ''Tag)

makeTag :: Bool -> Tag
makeTag start = Tag {_tagName="", _attrs=[], _selfClosing=False, _opening=start}

preProcess :: String -> String
preProcess = filter (/= '\r')

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

newtype Comment = Comment String deriving (Show, Eq)

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
    , _currentTagToken :: Tag
    , _currentCommentToken :: Comment
    , _currentDOCTYPEToken :: DOCTYPE
    , _temporaryBuffer :: String
    , _characterReferenceCode :: Int

    , _lastEmitted :: Token
    , _lastStart :: Maybe Tag
    , _emitted :: [Token]
} deriving (Show)
$(makeLenses ''State)

getNextInputCharacter :: State -> (State, Char)
getNextInputCharacter state =
    let out = head $ _input state
    in (over input (drop 1) state, out)

emitToken :: Token -> State -> State
emitToken (TagToken t) state = (if _opening t
    then set lastStart (Just t)
    else id) $ _emitToken (TagToken t) state
emitToken token state = _emitToken token state

_emitToken :: Token -> State -> State
_emitToken token state = over emitted (token:) $ set lastEmitted token state

doStateMachine :: State -> State
doStateMachine state = case _stateMachineState state of

    DataState -> case nextInputCharacter of
        '&' -> set returnState DataState (set stateMachineState CharacterReferenceState state')
        '<' -> set stateMachineState TagOpenState state'
        '\0' -> emitToken (Character nextInputCharacter) state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    RCDataState -> case nextInputCharacter of
        '&' -> set returnState RCDataState (set stateMachineState CharacterReferenceState state')
        '<' -> set stateMachineState RCDataLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    RawTextState -> case nextInputCharacter of
        '<' -> set stateMachineState RawTextLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    ScriptDataState -> case nextInputCharacter of
        '<' -> set stateMachineState ScriptDataLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    PlainTextState -> case nextInputCharacter of
        '\0' -> emitToken (Character '\xfffd') state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    TagOpenState
        | nextInputCharacter == '!' -> set stateMachineState MarkupDeclarationOpenState state'
        | nextInputCharacter == '/' -> set stateMachineState EndTagOpenState state'
        | isAlpha nextInputCharacter -> set stateMachineState TagNameState $ set currentTagToken (makeTag True) state
        | nextInputCharacter == '?' -> set stateMachineState BogusCommentState $ set currentCommentToken (Comment "") state
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (Character '<') state'
        | otherwise -> set stateMachineState DataState $ emitToken (Character '<') state

    EndTagOpenState
        | isAlpha nextInputCharacter -> set stateMachineState TagNameState $ set currentTagToken (makeTag False) state
        | nextInputCharacter == '>' -> set stateMachineState DataState state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (Character '/') $ emitToken (Character '<') state'
        | otherwise -> set stateMachineState BogusCommentState $ set currentCommentToken (Comment "") state

    TagNameState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeAttributeNameState state'
        | nextInputCharacter == '/' -> set stateMachineState SelfClosingStartTag state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\0' -> appendCharacter '\xfffd'
        | nextInputCharacter == '\xfffa' -> emitToken EOF state'
        | otherwise -> appendCharacter $ toLower nextInputCharacter

    RCDataLessThanSignState -> if nextInputCharacter == '/'
        then set stateMachineState RCDataEndTagOpenState (set temporaryBuffer "" state')
        else set stateMachineState RCDataState (emitToken (Character '<') state)

    RCDataEndTagOpenState -> if isAlpha nextInputCharacter
        then set stateMachineState RCDataEndTagNameState state
        else set stateMachineState RCDataState $ emitToken (Character '/') $ emitToken (Character '<') state

    RCDataEndTagNameState -> doEndNameState RCDataState

    RawTextLessThanSignState -> if nextInputCharacter == '/'
        then set stateMachineState RawTextEndTagOpenState (set temporaryBuffer "" state')
        else set stateMachineState RawTextState (emitToken (Character '<') state)

    RawTextEndTagOpenState -> if isAlpha nextInputCharacter
        then set stateMachineState RawTextEndTagNameState $ set currentTagToken (makeTag False) state
        else set stateMachineState RawTextState $ emitToken (Character '/') $ emitToken (Character '<') state

    RawTextEndTagNameState -> doEndNameState RawTextState

    ScriptDataLessThanSignState -> case nextInputCharacter of
        '/' -> set stateMachineState ScriptDataEndTagOpenState (set temporaryBuffer "" state')
        '!' -> emitToken (Character '!') $ emitToken (Character '<') $ set stateMachineState ScriptDataEscapeStartState state'
        _ -> set stateMachineState ScriptDataState (emitToken (Character '<') state)

    ScriptDataEndTagOpenState -> if isAlpha nextInputCharacter
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
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    ScriptDataEscapedDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataEscapedDashDashState state'
        '<' -> set stateMachineState ScriptDataEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataEscapedState state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataEscapedState state'

    ScriptDataEscapedDashDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') state'
        '<' -> set stateMachineState ScriptDataEscapedLessThanSignState state'
        '>' -> emitToken (Character '>') $ set stateMachineState ScriptDataState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataEscapedState state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataEscapedState state'

    ScriptDataEscapedLessThanSignState
        | nextInputCharacter == '/' -> set stateMachineState ScriptDataEscapedEndTagOpenState $ set temporaryBuffer "" state'
        | isAlpha nextInputCharacter -> set stateMachineState ScriptDataDoubleEscapeStartState $ emitToken (Character '<') $ set temporaryBuffer "" state
        | otherwise -> set stateMachineState ScriptDataEscapedState $ emitToken (Character '<') state

    ScriptDataEscapedEndTagOpenState -> if isAlpha nextInputCharacter
        then set stateMachineState ScriptDataEscapedEndTagNameState $ set currentTagToken (makeTag False) state
        else set stateMachineState ScriptDataEscapedState $ emitToken (Character '/') $ emitToken (Character '<') state

    ScriptDataEscapedEndTagNameState -> doEndNameState ScriptDataEscapedState

    ScriptDataDoubleEscapeStartState
        | nextInputCharacter `elem` "\t\n\f />" -> emitToken (Character nextInputCharacter) $ set stateMachineState (if _temporaryBuffer state' == "script"
            then ScriptDataDoubleEscapedState
            else ScriptDataEscapedState) state'
        | isAlphaUpper nextInputCharacter -> emitToken (Character nextInputCharacter) $ over temporaryBuffer (++[toLower nextInputCharacter]) state'
        | otherwise -> set stateMachineState ScriptDataEscapedState state

    ScriptDataDoubleEscapedState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataDoubleEscapedDashState state'
        '<' -> emitToken (Character '<') $ set stateMachineState ScriptDataDoubleEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    ScriptDataDoubleEscapedDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') $ set stateMachineState ScriptDataDoubleEscapedDashDashState state'
        '<' -> set stateMachineState ScriptDataDoubleEscapedLessThanSignState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataDoubleEscapedState state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataDoubleEscapedState state'

    ScriptDataDoubleEscapedDashDashState -> case nextInputCharacter of
        '-' -> emitToken (Character '-') state'
        '<' -> emitToken (Character '<') $ set stateMachineState ScriptDataDoubleEscapedLessThanSignState state'
        '>' -> emitToken (Character '>') $ set stateMachineState ScriptDataState state'
        '\0' -> emitToken (Character '\xfffd') $ set stateMachineState ScriptDataDoubleEscapedState state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) $ set stateMachineState ScriptDataDoubleEscapedState state'

    ScriptDataDoubleEscapedLessThanSignState -> if nextInputCharacter == '/'
        then emitToken (Character '/') $ set stateMachineState ScriptDataDoubleEscapeEndState $ set temporaryBuffer "" state'
        else set stateMachineState ScriptDataDoubleEscapedState state

    ScriptDataDoubleEscapeEndState
        | nextInputCharacter `elem` "\t\n\f />" -> emitToken (Character nextInputCharacter) $ set stateMachineState (if _temporaryBuffer state' == "script"
            then ScriptDataEscapedState
            else ScriptDataDoubleEscapedState) state'
        | isAlphaUpper nextInputCharacter -> emitToken (Character nextInputCharacter) $ over temporaryBuffer (++[toLower nextInputCharacter]) state'
        | otherwise -> set stateMachineState ScriptDataDoubleEscapedState state

    BeforeAttributeNameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter `elem` "/>#" -> set stateMachineState AfterAttributeNameState state
        | nextInputCharacter == '=' -> set stateMachineState AttributeNameState $ startAttribute state'
        | otherwise -> set stateMachineState AttributeNameState $ startAttribute state

    AttributeNameState -- if duplicate, should remove the new one
        | nextInputCharacter `elem` "\t\n\f />#" -> set stateMachineState AfterAttributeNameState state
        | nextInputCharacter == '=' -> set stateMachineState BeforeAttributeValueState state'
        | nextInputCharacter == '\0' -> appendToAttrName '\xfffd' state'
        | otherwise -> appendToAttrName (toLower nextInputCharacter) state'

    AfterAttributeNameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '/' -> set stateMachineState SelfClosingStartTag state'
        | nextInputCharacter == '=' -> set stateMachineState BeforeAttributeValueState state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF state'
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
        '\xfffa' -> emitToken EOF state'
        _ -> appendToAttr nextInputCharacter state'

    AttributeValueSingleQuotedState -> case nextInputCharacter of
        '\'' -> set stateMachineState AfterAttributeValueQuotedState state'
        '&' -> set stateMachineState CharacterReferenceState $ set returnState AttributeValueSingleQuotedState state'
        '\0' -> appendToAttr '\xfffd' state'
        '\xfffa' -> emitToken EOF state'
        _ -> appendToAttr nextInputCharacter state'

    AttributeValueUnquotedState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeAttributeNameState state'
        | nextInputCharacter == '&' -> set stateMachineState CharacterReferenceState $ set returnState AttributeValueUnquotedState state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\0' -> emitToken (Character '\xfffd') state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF state'
        | otherwise -> appendToAttr nextInputCharacter state'

    AfterAttributeValueQuotedState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeAttributeNameState state'
        | nextInputCharacter == '/' -> set stateMachineState SelfClosingStartTag state'
        | nextInputCharacter == '>' -> emitToken (TagToken $ _currentTagToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF state'
        | otherwise -> set stateMachineState BeforeAttributeNameState state

    SelfClosingStartTag -> case nextInputCharacter of
        '>' ->
            let state'' = over currentTagToken (set selfClosing True) state'
            in emitToken (TagToken $ _currentTagToken state'') $ set stateMachineState DataState state''
        '\xfffa' -> emitToken EOF state'
        _ -> set stateMachineState BeforeAttributeNameState state

    BogusCommentState -> case nextInputCharacter of
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '\xfffa' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        '\0' -> appendToComment '\xfffd' state'
        _ -> appendToComment nextInputCharacter state'

    MarkupDeclarationOpenState
        | outComment -> set stateMachineState CommentStartState $ set input restComment $ set currentCommentToken (Comment "") state
        | outDoctype -> set stateMachineState DOCTYPEState $ set input restDoctype state
        | outCData -> set stateMachineState BogusCommentState $ set input restCData $ set currentCommentToken (Comment "") state
        | otherwise -> set stateMachineState BogusCommentState $ set currentCommentToken (Comment "") state
        where
            (restComment, outComment) = checkWord "--"
            (restDoctype, outDoctype) = checkWordIgnoreCase "DOCTYPE"
            (restCData, outCData) = checkWord "[CDATA["

    CommentStartState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentStartDashState state'
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        _ -> set stateMachineState CommentState state

    CommentStartDashState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentEndState state'
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '\xfffa' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '-' state'

    CommentState -> case nextInputCharacter of
        '<' -> set stateMachineState CommentLessThanSignState $ appendToComment nextInputCharacter state'
        '-' -> set stateMachineState CommentEndDashState state'
        '\0' -> appendToComment '\xfffd' state'
        '\xfffa' -> emitToken EOF $ emitToken (Character nextInputCharacter) state'
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
        '\xfffa' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '-' state

    CommentEndState -> case nextInputCharacter of
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '!' -> set stateMachineState CommentEndBangState state'
        '-' -> appendToComment '-' state'
        '\xfffa' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '-' $ appendToComment '-' state

    CommentEndBangState -> case nextInputCharacter of
        '-' -> set stateMachineState CommentEndDashState $ appendToComment '!' $ appendToComment '-' $ appendToComment '-' state'
        '>' -> emitToken (CommentToken $ _currentCommentToken state') $ set stateMachineState DataState state'
        '\xfffa' -> emitToken EOF $ emitToken (CommentToken $ _currentCommentToken state') state'
        _ -> set stateMachineState CommentState $ appendToComment '!' $ appendToComment '-' $ appendToComment '-' state

    DOCTYPEState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeDOCTYPENameState state'
        | nextInputCharacter == '>' -> set stateMachineState BeforeDOCTYPENameState state
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag $ createDoctype state'
        | otherwise -> set stateMachineState BeforeDOCTYPENameState state

    BeforeDOCTYPENameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '\0' -> set stateMachineState DOCTYPENameState $ ahhhh '\xfffd' $ createDoctype state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag $ createDoctype state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag $ createDoctype state'
        | otherwise -> set stateMachineState DOCTYPENameState $ ahhhh (toLower nextInputCharacter) $ createDoctype state'
        where ahhhh c = over currentDOCTYPEToken (set name $ Just [c])

    DOCTYPENameState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState AfterDOCTYPENameState state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\0' -> appendToDOCTYPE '\xfffd' name state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> appendToDOCTYPE (toLower nextInputCharacter) name state'

    AfterDOCTYPENameState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | outPublic -> set stateMachineState AfterDOCTYPEPublicKeywordState $ set input restPublic state
        | outSystem -> set stateMachineState AfterDOCTYPESystemKeywordState $ set input restSystem state
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state
        where
            (restPublic, outPublic) = checkWord "public"
            (restSystem, outSystem) = checkWord "system"

    AfterDOCTYPEPublicKeywordState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeDOCTYPEPublicIdentifierState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPEPublicIdentifierDoubleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPEPublicIdentifierSingleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state

    BeforeDOCTYPEPublicIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPEPublicIdentifierDoubleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPEPublicIdentifierSingleQuotedState $ setDOCTYPEInitial public state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state

    DOCTYPEPublicIdentifierDoubleQuotedState -> case nextInputCharacter of
        '"' -> set stateMachineState AfterDOCTYPEPublicIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' public state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter public state'

    DOCTYPEPublicIdentifierSingleQuotedState -> case nextInputCharacter of
        '\'' -> set stateMachineState AfterDOCTYPEPublicIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' public state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter public state'

    AfterDOCTYPEPublicIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BetweenDOCTYPEPublicAndSystemIdentifiersState state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state

    BetweenDOCTYPEPublicAndSystemIdentifiersState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state

    AfterDOCTYPESystemKeywordState
        | nextInputCharacter `elem` "\t\n\f " -> set stateMachineState BeforeDOCTYPESystemIdentifierState state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state'

    BeforeDOCTYPESystemIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '"' -> set stateMachineState DOCTYPESystemIdentifierDoubleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '\'' -> set stateMachineState DOCTYPESystemIdentifierSingleQuotedState $ setDOCTYPEInitial system state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState $ setDOCTYPEFlag state

    DOCTYPESystemIdentifierDoubleQuotedState -> case nextInputCharacter of
        '"' -> set stateMachineState AfterDOCTYPESystemIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' system state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter system state'

    DOCTYPESystemIdentifierSingleQuotedState -> case nextInputCharacter of
        '\'' -> set stateMachineState AfterDOCTYPESystemIdentifierState state'
        '\0' -> appendToDOCTYPE '\xfffd' system state'
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState $ setDOCTYPEFlag state'
        '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        _ -> appendToDOCTYPE nextInputCharacter system state'

    AfterDOCTYPESystemIdentifierState
        | nextInputCharacter `elem` "\t\n\f " -> state'
        | nextInputCharacter == '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        | nextInputCharacter == '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ setDOCTYPEFlag state'
        | otherwise -> set stateMachineState BogusDOCTYPEState state

    BogusDOCTYPEState -> case nextInputCharacter of
        '>' -> emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') $ set stateMachineState DataState state'
        '\xfffa' -> emitToken EOF $ emitToken (DOCTYPEToken $ _currentDOCTYPEToken state') state'
        _ -> state'

    CDATASectionState -> case nextInputCharacter of
        ']' -> set stateMachineState CDATASectionBracketState state'
        '\xfffa' -> emitToken EOF state'
        _ -> emitToken (Character nextInputCharacter) state'

    CDATASectionBracketState -> if nextInputCharacter == ']'
        then set stateMachineState CDATASectionEndState state'
        else set stateMachineState CDATASectionState $ emitToken (Character ']') state

    CDATASectionEndState -> case nextInputCharacter of
        ']' -> emitToken (Character ']') state'
        '>' -> set stateMachineState DataState state'
        _ -> set stateMachineState CDATASectionState $ emitToken (Character ']') $ emitToken (Character ']') state

    CharacterReferenceState
        | isAlphanumeric nextInputCharacter -> set stateMachineState NamedCharacterReferenceState $ setBuf state
        | nextInputCharacter == '\xfffa' -> set stateMachineState NumericCharacterReferenceState $ appendToTemporaryBuffer [nextInputCharacter] $ setBuf state'
        | otherwise -> set stateMachineState (_returnState state') $ flushCodePoints $ setBuf state'
        where setBuf = set temporaryBuffer "&"

    NamedCharacterReferenceState -> if not $ null checked
        then if partOfAnAttribute state' && not semicoloned && (nextInputCharacter == '=' || isAlphanumeric nextInputCharacter)
            then set stateMachineState (_returnState state') $ flushCodePoints state'
            else set stateMachineState (_returnState state) $ flushCodePoints $ set temporaryBuffer [characterReferences ! om] $ set input rester state
        else set stateMachineState AmbiguousAmpersandState $ flushCodePoints state'
        where
            checked = filter (\ key -> key == take (length key) (_input state)) $ keys characterReferences
            om = last $ sortOn length checked
            rest = drop (length om) (_input state)
            semicoloned = head rest == ';'
            rester = drop (if semicoloned then 1 else 0) rest

    AmbiguousAmpersandState -> if isAlphanumeric nextInputCharacter
        then if partOfAnAttribute state'
            then appendToAttr nextInputCharacter state'
            else emitToken (Character nextInputCharacter) state'
        else set stateMachineState (_returnState state) state

    NumericCharacterReferenceState -> if toLower nextInputCharacter == 'x'
        then set stateMachineState HexadecimalCharacterReferenceStartState $ appendToTemporaryBuffer [nextInputCharacter] $ getBuf state'
        else set stateMachineState DecimalCharacterReferenceStartState $ getBuf state
        where getBuf = set characterReferenceCode 0

    HexadecimalCharacterReferenceStartState -> if isHexDigit nextInputCharacter
        then set stateMachineState HexadecimalCharacterReferenceState state
        else set stateMachineState (_returnState state') $ flushCodePoints state

    DecimalCharacterReferenceStartState -> if isDigit nextInputCharacter
        then set stateMachineState DecimalCharacterReferenceState state
        else set stateMachineState (_returnState state') $ flushCodePoints state

    HexadecimalCharacterReferenceState
        | isHexDigit nextInputCharacter -> over characterReferenceCode (\ n -> n * 16 + hexToInt nextInputCharacter) state'
        | nextInputCharacter == ';' -> set stateMachineState NumericCharacterReferenceEndState state'
        | otherwise -> set stateMachineState NumericCharacterReferenceEndState state

    DecimalCharacterReferenceState
        | isHexDigit nextInputCharacter -> over characterReferenceCode (\ n -> n * 10 + read [nextInputCharacter]) state'
        | nextInputCharacter == ';' -> set stateMachineState NumericCharacterReferenceEndState state'
        | otherwise -> set stateMachineState NumericCharacterReferenceEndState state

    NumericCharacterReferenceEndState
        | code == 0 || code > 0x10ffff || isSurrogate code -> set characterReferenceCode 0xfffd state
        | code == 0x0d || (isControl code && not (isWhitespace code)) -> set characterReferenceCode (errToCodePoint ! fromIntegral code) state
        | otherwise -> set stateMachineState (_returnState state) $ flushCodePoints $ set temporaryBuffer [chr code] state
        where code = _characterReferenceCode state

    where
        startAttribute = over currentTagToken (over attrs (Attribute ("", ""):))

        partOfAnAttribute s = _returnState s `elem` [AttributeValueDoubleQuotedState, AttributeValueSingleQuotedState, AttributeValueUnquotedState]

        flushCodePoints s = set temporaryBuffer "" (if partOfAnAttribute s
            then s
            else foldr (emitToken . Character) s (_temporaryBuffer s))

        appendToTemporaryBuffer c = over temporaryBuffer (++c)

        createDoctype = set currentDOCTYPEToken DOCTYPE {_enableQuirksFlag=False, _name=Nothing, _system=Nothing, _public=Nothing}

        setDOCTYPEFlag = over currentDOCTYPEToken (set enableQuirksFlag True)

        setDOCTYPEInitial setter = over currentDOCTYPEToken (set setter $ Just "")

        appendToDOCTYPE toAppend setter = over currentDOCTYPEToken (over setter doAppend)
            where
                doAppend (Just str) = Just $ str ++ [toAppend]
                doAppend Nothing = Just [toAppend]

        appendToAttrName = over currentTagToken . over attrs . doThing
            where
                doThing toAppend a = doThinger toAppend (head a) : drop 1 a
                doThinger toAppend (Attribute attr) = Attribute (fst attr ++ [toAppend], snd attr)

        appendToAttr = over currentTagToken . over attrs . doThing
            where
                doThing toAppend a = doThinger toAppend (head a) : drop 1 a
                doThinger toAppend (Attribute attr) = Attribute (fst attr, snd attr ++ [toAppend])

        appendToComment c = over currentCommentToken (\ (Comment thing) -> Comment $ thing ++ [c])

        (state', nextInputCharacter) = getNextInputCharacter state

        appropriateEndTagToken = case _lastStart state of
            Just t -> _tagName t == _tagName (_currentTagToken state)
            Nothing -> False

        appendCharacter c = over currentTagToken (over tagName (++[c])) state'

        doEndNameState endState
            | appropriateEndTagToken && nextInputCharacter `elem` "\t\n\f " = set stateMachineState BeforeAttributeNameState state'
            | appropriateEndTagToken && nextInputCharacter == '/' = set stateMachineState SelfClosingStartTag state'
            | appropriateEndTagToken && nextInputCharacter == '>' = emitToken (TagToken $ _currentTagToken state') (set stateMachineState DataState state')
            | isAlpha nextInputCharacter = over temporaryBuffer (++[toLower nextInputCharacter]) $ appendCharacter nextInputCharacter
            | otherwise = set stateMachineState endState (foldr (emitToken . Character) (emitToken (Character '/') (emitToken (Character '<') state)) (_temporaryBuffer state))

        hexToInt c = read $ "0x" ++ [c]

        checkWord w =
            let (first, rest) = splitAt (length w) (_input state)
            in (rest, first == w)

        checkWordIgnoreCase w =
            let (first, rest) = splitAt (length w) (_input state)
            in (rest, map toLower first == map toLower w)


killCommentsWhat :: State -> State
killCommentsWhat = over emitted $ filter $ \case
    (CommentToken _) -> False
    _ -> True

parseString :: String -> State
parseString str =
    killCommentsWhat $ over emitted reverse $ _parseString State {
        _stateMachineState = DataState
        , _returnState = DataState
        , _input = preProcess str ++ "\xfffa"
        , _currentTagToken = makeTag False
        , _currentCommentToken = Comment ""
        , _currentDOCTYPEToken = DOCTYPE {_enableQuirksFlag=False, _name=Nothing, _system=Nothing, _public=Nothing}
        , _temporaryBuffer = ""
        , _characterReferenceCode = 0

        , _lastEmitted = Character 'a'
        , _lastStart = Nothing
        , _emitted = []
    }

_parseString :: State -> State
_parseString state = if _lastEmitted state /= EOF
    then _parseString $ doStateMachine state
    else state
