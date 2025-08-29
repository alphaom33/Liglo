module CSSParserTest where

import Test.HUnit

import CSSParser
import CSSTokenizer (CSSToken(..))

testKillrest = TestList [
        TestLabel "basic" $ TestCase $ assertEqual "goshMate" [EOFToken] $ killrest [ClosingCurlyBracketToken, ClosingCurlyBracketToken, EOFToken]
        , TestLabel "two" $ TestCase $ assertEqual "goshMate" [EOFToken] $ killrest [OpeningCurlyBracketToken, ClosingCurlyBracketToken, OpeningCurlyBracketToken, IdentToken "hiya", ClosingCurlyBracketToken, ClosingCurlyBracketToken, ClosingCurlyBracketToken, EOFToken]
        , TestLabel "lots" $ TestCase $ assertEqual "goshMate" [EOFToken] $ killrest [OpeningCurlyBracketToken,ClosingCurlyBracketToken,IdentToken ".summary",IdentToken "section",OpeningSquareBracketToken,IdentToken "class",DelimToken '$',DelimToken '=',StringToken "-summary",ClosingSquareBracketToken,CommaToken,IdentToken ".details",IdentToken "section",OpeningSquareBracketToken,IdentToken "class",DelimToken '$',DelimToken '=',StringToken "-details",ClosingSquareBracketToken,CommaToken,IdentToken ".class-uses",IdentToken ".detail",CommaToken,IdentToken ".serialized-class-details",OpeningCurlyBracketToken,IdentToken "padding",ColonToken,NumberToken 0.0,DimensionToken (8.0,"px"),DimensionToken (5.0,"px"),DimensionToken (8.0,"px"),SemicolonToken,ClosingCurlyBracketToken,ClosingCurlyBracketToken,ClosingCurlyBracketToken,EOFToken]
    ]

testMatchstar = TestList [
        TestLabel "basic" $ assertMatch True "asdf" "asdf"
        , TestLabel "yep" $ assertMatch True "a" "asdf"
        , TestLabel "nope" $ assertMatch False "a" ""
    ]
    where 
        assertMatch t c v = TestCase $ assertEqual "" t $ matchStar c v

testMatchlist = TestList [
        TestLabel "basic" $ assertMatch True "asdf" "asdf"
        , TestLabel "yep" $ assertMatch True "asdf" "a asdf b"
        , TestLabel "nope" $ assertMatch False "asdf" "a b asd f"
    ]
    where
        assertMatch t c v = TestCase $ assertEqual "" t $ matchspaced c v

testMatchCombinator = TestList [
        TestLabel "childBasic" $ TestCase $ assertEqual "" (ChildCombinator, []) $ matchCombinator [DelimToken '>']
        , TestLabel "childWhite" $ TestCase $ assertEqual "" (ChildCombinator, [WhitespaceToken]) $ matchCombinator [WhitespaceToken, DelimToken '>', WhitespaceToken]
        , TestLabel "white" $ TestCase $ assertEqual "" (DescendantCombinator, [IdentToken ""]) $ matchCombinator [WhitespaceToken, IdentToken ""]
        , TestLabel "whiteEnd" $ TestCase $ assertEqual "" (CurrentCombinator, []) $ matchCombinator [WhitespaceToken]
        , TestLabel "current" $ TestCase $ assertEqual "" (CurrentCombinator, [IdentToken ""]) $ matchCombinator [IdentToken ""]
        , TestLabel "currentEnd" $ TestCase $ assertEqual "" (CurrentCombinator, []) $ matchCombinator []
    ]

tests = TestList [
    TestLabel "killrest" testKillrest
    , TestLabel "matchstar" testMatchstar
    , TestLabel "matchlist" testMatchlist
    , TestLabel "matchCombinator" testMatchCombinator
    ]
