module HTMLBuilderTest where

import Test.HUnit
import HTMLBuilder (blamCSS)

blamCSSTest = TestList [
        TestLabel "mhm" $ TestCase $ assertEqual "" [] $ blamCSS []
    ]

tests = TestList [

    ]
