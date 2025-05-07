module Main where

import Test.HUnit
import qualified System.Exit as Exit
import qualified HTMLParser as H
import Data.Either (isRight, isLeft)

testState :: H.State -> H.InsertionMode -> Test
testState state endMode = TestCase (assertEqual "wrong mode, idiot" endMode (H._mode (H.resetInsertionMode state)))

testMode :: [H.TagType] -> H.InsertionMode -> Test
testMode tagTypes endMode = 
    let 
        initial = H.State {
            H._openElements = map (\x -> H.Tag {H.tagType=x, H.attrs=[], H.selfClosing=False}) tagTypes
            , H._mode = H.Initial
            , H._templateModes = [H.TestMode]
            , H._headPointer = Nothing
            , H._formPointer = Nothing}
    in
        testState initial endMode

testResetInsertionMode :: Test
testResetInsertionMode = TestList $ [
        TestLabel "Select1" $ testMode [H.Select] H.InSelect,
        TestLabel "Select2" $ testMode [H.Select, H.Select] H.InSelect,
        TestLabel "SelectTable" $ testMode [H.Select, H.Table, H.Table] H.InSelectInTable,
        TestLabel "SelectTemplate" $ testMode [H.Select, H.Template, H.Table, H.Table] H.InSelect,
        TestLabel "TDTrue" $ testMode [H.TD, H.TestType] H.InCell,
        TestLabel "THTrue" $ testMode [H.TD, H.TestType] H.InCell,
        TestLabel "TDFalse" $ testMode [H.TD] H.InBody,
        TestLabel "THFalse" $ testMode [H.TH] H.InBody,
        TestLabel "TBody" $ testMode [H.TBody] H.InTableBody,
        TestLabel "THead" $ testMode [H.THead] H.InTableBody,
        TestLabel "TFoot" $ testMode [H.TFoot] H.InTableBody,
        TestLabel "TR" $ testMode [H.TR] H.InRow,
        TestLabel "Caption" $ testMode [H.Caption] H.InCaption,
        TestLabel "Colgroup" $ testMode [H.Colgroup] H.InColgroup,
        TestLabel "Table" $ testMode [H.Table] H.InTable,
        TestLabel "Template" $ testMode [H.Template] H.TestMode,
        TestLabel "Head" $ testMode [H.Head] H.InHead,
        TestLabel "Body" $ testMode [H.Body] H.InBody,
        TestLabel "FrameSet" $ testMode [H.FrameSet] H.InFrameSet,
        TestLabel "Nothing" $ testMode [H.TestType] H.InBody
    ] ++ (let initial headPointer = H.State {
            H._openElements = [H.Tag {H.tagType=H.Html, H.attrs=[], H.selfClosing=False}]
            , H._mode = H.Initial
            , H._templateModes = []
            , H._headPointer = headPointer
            , H._formPointer = Nothing}
            in 
        [TestLabel "HTMLHead" $ testState (initial Nothing) H.BeforeHead,
        TestLabel "HTMLBody" $ testState (initial $ Just H.Tag {H.tagType=H.TestType, H.attrs=[], H.selfClosing=False}) H.AfterHead])
        
assertParse toParse check text = TestCase $ let out = (H.parse toParse text) in if check $ snd out
    then assertEqual "Yay" 0 0
    else assertFailure $ show out

testComment :: Test
testComment = TestList $ [
        TestLabel "Basic" $ makeCommentTest isRight "<!--aaa-->"
        , TestLabel "Nothing" $ makeCommentTest isRight "<!---->"
    ]
    where makeCommentTest = assertParse H.comment
        
testWhiteSpace :: Test
testWhiteSpace = TestList $ [
        TestLabel "Basic" $ assertParse H.whitespace isRight " \r\n\r\t\f" 
    ]
 
testDoctype :: Test
testDoctype = TestList $ [
        TestLabel "Checkacy" $ assertParse H.legacy isRight " SYSTEM 'about:legacy-compat'",
        TestLabel "Normal" $ assertParse H.doctype isRight "<!DOCTYPE html>",
        TestLabel "Whitespace" $ assertParse H.doctype isRight "<!DOCTYPE   html  >",
        TestLabel "Casey" $ assertParse H.doctype isRight "<!dOCtyPE HtmL>",
        TestLabel "Legacy" $ assertParse H.doctype isRight "<!DOCTYPE html SYSTEM 'about:legacy-compat'>",
        TestLabel "LegacyDoubles" $ assertParse H.doctype isRight "<!DOCTYPE html SYSTEM \"about:legacy-compat\">",
        TestLabel "LegacyOthers" $ assertParse H.doctype isLeft "<!DOCTYPE html SYSTEM 'about:legacy-compat\">",
        TestLabel "LegacyWhitespace" $ assertParse H.doctype isRight "<!DOCTYPE  html   SYSTEM \n'about:legacy-compat'\r  >",
        TestLabel "LegacyCase" $ assertParse H.doctype isRight "<!DOctYPE htMl SYSteM 'about:legacy-compat'>"
    ]
 
testStartTag = TestList $ [
        TestLabel "Basic" $ testStart isRight "<td>"
        , TestLabel "BasicCase" $ thing H.Tag {H.tagType=H.TD, H.attrs=[], H.selfClosing=False} "<Td>"
        , TestLabel "BasicSolidus" $ testStart isLeft "<td/>"
        , TestLabel "BasicWhitespace" $ testStart isRight "<td   >"
        , TestLabel "Void" $ testStart isRight "<area>"
        , TestLabel "VoidSolidus" $ testStart isRight "<area/>"
        , TestLabel "VoidWhitespace" $ testStart isRight "<area \r/>"
        , TestLabel "Foreign" $ thing1 False "<svg>"
        , TestLabel "ForeignSelf" $ thing1 True "<svg/>"
        , TestLabel "ForeignWhitespace" $ thing1 True "<svg />"
    ]
    where
        thing1 selfClosing = thing H.Tag {H.tagType=H.SVG, H.attrs=[], H.selfClosing=selfClosing}
        thing against str = TestCase $ case H.parse H.startTag str of
            (_, Right a) -> assertEqual "nequal, dumbo" against a
            a -> assertFailure $ show a
        testStart = assertParse H.startTag
 
tests = TestList $ [
    TestLabel "ResetInsertionMode" testResetInsertionMode,
    TestLabel "Comment" testComment,
    TestLabel "Whitespace" testWhiteSpace,
    TestLabel "Doctype" testDoctype,
    TestLabel "StartTag" testStartTag
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
