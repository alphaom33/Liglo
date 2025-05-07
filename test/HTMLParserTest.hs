module Main where

import Test.HUnit
import qualified System.Exit as Exit
import qualified HTMLParser as H

testState :: H.State -> H.InsertionMode -> Test
testState state endMode = TestCase (assertEqual "wrong mode, idiot" endMode (H._mode (H.resetInsertionMode state)))

testMode :: [H.TagType] -> H.InsertionMode -> Test
testMode tagTypes endMode = 
    let 
        initial = H.State {
            H._openElements = map (\x -> H.Tag {H.tagType=x, H.attrs=[]}) tagTypes
            , H._mode = H.Initial
            , H._templateModes = [H.TestMode]
            , H._headPointer = Nothing
            , H._formPointer = Nothing}
    in
        testState initial endMode

tests :: Test
tests = TestList $ [
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
            H._openElements = [H.Tag {H.tagType=H.Html, H.attrs=[]}]
            , H._mode = H.Initial
            , H._templateModes = []
            , H._headPointer = headPointer
            , H._formPointer = Nothing}
            in 
        [TestLabel "HTMLHead" $ testState (initial Nothing) H.BeforeHead,
        TestLabel "HTMLBody" $ testState (initial $ Just H.Tag {H.tagType=H.TestType, H.attrs=[]}) H.AfterHead])

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
