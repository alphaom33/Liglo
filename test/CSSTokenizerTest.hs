module CSSTokenizerTest where

import Test.HUnit
import qualified System.Exit as Exit
import HTMLParser
import Data.Either (isRight, isLeft, fromRight)

import CSSTokenizer

assertSucceeded message thingy str = assertBool message $ isRight $ snd $ parse thingy str
assertFailed message thingy str = assertBool message $ isLeft $ snd $ parse thingy str

matchNotStringTest = TestList [
    TestLabel "notString" $ TestCase $ assertSucceeded "Should work what" (matchNotString "asdf") "amdf"
    , TestLabel "yepString" $ TestCase $ assertFailed "Should work what" (matchNotString "asdf") "asdf"
    ]

matchIdentTest = TestList [
    TestLabel "dot" $ TestCase $ assertEqual "mhm" "a" (fromRight "" $ snd $ parse consumeIdentSequence "a.a")
    , TestLabel "yep" $ TestCase $ assertEqual "mhm" (ClassToken "a") (fromRight (ClassToken "") $ snd $ parse consumeClass ".a")
    ]

tests = TestList [
    TestLabel "matchNotString" matchNotStringTest
    , TestLabel "matchIdent" matchIdentTest
    ]
