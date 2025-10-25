module Main where

import Test.HUnit
import qualified System.Exit as Exit
import HTMLParser
import Data.Either (isRight, isLeft)

import qualified HTMLParserTest as H
import qualified HTMLBuilderTest as HB
import qualified CSSTokenizerTest as T
import qualified CSSParserTest as P
import qualified MortarTest as M

import StyleStealer

nomUrlTests = TestList [
        TestLabel "normal" $ TestCase $ assertEqual "idiot" "asdf/bsdf.mhm" $ resolveUrl "asdf" "bsdf.mhm"
        , TestLabel "up" $ TestCase $ assertEqual "idiot" "asdf/bsdf.mhm" $ resolveUrl "asdf/dsdf/csdf.yep" "../bsdf.mhm"
    ]

main :: IO ()
main = do
    result <- runTestTT $ TestList [
        TestLabel "HTMLParserTest" H.tests
        , TestLabel "CSSTokenizerTest" T.tests
        , TestLabel "CSSParserTest" P.tests
        , TestLabel "NomURLTest" nomUrlTests
        , TestLabel "HTMLBuilderTest" HB.tests
        , TestLabel "MortarTest" M.tests
        ]
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
