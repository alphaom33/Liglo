module HTMLParserTest where

import Test.HUnit
import qualified System.Exit as Exit
import HTMLParser
import Data.Either (isRight, isLeft)

testSelfClosing = TestList [
    TestLabel "basic" $ TestCase (assertEqual "" (_emitted (parseString "<meta />")) [
        TagToken $ Tag {_tagName="meta", _selfClosing=True, _opening=True, _attrs=[]}
        , EOF
        ])
    , TestLabel "attrs" $ TestCase (assertEqual "" (_emitted (parseString "<meta asdf=\"mhm\"/>")) [
        TagToken $ Tag {_tagName="meta", _selfClosing=True, _opening=True, _attrs=[Attribute ("asdf", "mhm")]}
        , EOF
        ])
    ]

tests = TestList [
    TestLabel "TestSelfClosing" testSelfClosing
    ]
