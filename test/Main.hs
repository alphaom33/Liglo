module Main where

import Test.HUnit
import qualified System.Exit as Exit
import HTMLParser
import Data.Either (isRight, isLeft)

import qualified HTMLParserTest as H
import qualified CSSTokenizerTest as T

main :: IO ()
main = do
    result <- runTestTT $ TestList $ [
        TestLabel "HTMLParserTest" H.tests
        , TestLabel "CSSTokenizerTest" T.tests
        ]
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
