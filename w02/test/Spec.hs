import Test.Hspec
--import Control.Exception (evaluate)
import Log
import LogAnalysis

main :: IO ()
main = hspec $ do
    describe "parseMessage" $ do
        it "returns Unknown for random" $
            parseMessage "abc" `shouldBe` Unknown "abc"

        it "returns Unknown for no timestamp" $
            parseMessage "E 2" `shouldBe` Unknown "E 2"

        it "returns correct for minimal error" $
            parseMessage "E 2 1" `shouldBe` LogMessage (Error 2) 1 ""

        it "returns correct for expected error format" $
            parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

        it "returns correct for expected info format" $
            parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    describe "parseMessageWithType" $ do
        it "should return Nothing if too short" $
            parseMessageWithType [] `shouldBe` Nothing

        it "should return Nothing if invalid error" $
            parseMessageWithType ["E", "A", "rest"] `shouldBe` Nothing

        it "should return Error" $
            parseMessageWithType ["E", "1", "rest"] `shouldBe` Just ((Error 1), ["rest"])

        it "should return Warning" $
            parseMessageWithType ["W", "rest"] `shouldBe` Just (Warning, ["rest"])

        it "should return Info" $
            parseMessageWithType ["I", "rest"] `shouldBe` Just (Info, ["rest"])

    describe "parse" $ do
        it "should return empty if empty" $
            parse "" `shouldBe` []

        it "should return single line" $
            parse "E A rest" `shouldBe` [Unknown "E A rest"]

        it "should return multi line" $
            parse "E A rest\nI 29 la" `shouldBe` [(Unknown "E A rest"), (LogMessage Info 29 "la")]

    describe "insertLog" $ do
        it "should return existing tree if given unknown" $
            let testTree = Node Leaf (LogMessage Info 10 "la") Leaf
            in insertLog (Unknown "abc") testTree `shouldBe` testTree

        it "should return new tree if given leaf" $
            let testMessage = LogMessage Info 0 "abc"
            in insertLog testMessage Leaf `shouldBe` Node Leaf testMessage Leaf

        it "should insert less than for a simple tree" $
            let
                testMessage = LogMessage Info 0 "abc"
                treeMessage = (LogMessage Info 10 "la")
                testTree = Node Leaf treeMessage Leaf
                expectedTree = Node (Node Leaf testMessage Leaf) treeMessage Leaf
            in
                insertLog testMessage testTree `shouldBe` expectedTree

        it "should insert greater than for a simple tree" $
            let
                testMessage = LogMessage Info 20 "abc"
                treeMessage = (LogMessage Info 10 "la")
                testTree = Node Leaf treeMessage Leaf
                expectedTree = Node Leaf treeMessage (Node Leaf testMessage Leaf)
            in
                insertLog testMessage testTree `shouldBe` expectedTree

    describe "buildLog" $ do
        it "should return leaf for empty messages" $
            buildLog [] `shouldBe` Leaf

        it "should return single node for single list" $
            let testMessage = LogMessage Info 0 "la"
            in buildLog [testMessage] `shouldBe` Node Leaf testMessage Leaf

        it "should return correct for larger tree" $
            let
                m0 = LogMessage Info 0 "0"
                m1 = LogMessage Info 1 "1"
                m2 = LogMessage Info 2 "2"
                m3 = LogMessage Info 3 "3"
                unknown = Unknown "la"
                testMessages = [unknown, m0, m2, m1, unknown, m3]
            in
                buildLog testMessages `shouldBe` Node (Node (Node Leaf m0 Leaf) m1 (Node Leaf m2 Leaf)) m3 Leaf

    describe "inOrder" $ do
        it "should return empty list for leaf" $
            inOrder Leaf `shouldBe` []

        it "should return single list for single node" $
            let testMessage = LogMessage Info 0 "0"
            in inOrder (Node Leaf testMessage Leaf) `shouldBe` [testMessage]

        it "should return proper list for larger tree" $
            let
                m0 = LogMessage Info 0 "0"
                m1 = LogMessage Info 1 "1"
                m2 = LogMessage Info 2 "2"
                m3 = LogMessage Info 3 "3"
                tree = Node (Node (Node Leaf m0 Leaf) m1 (Node Leaf m2 Leaf)) m3 Leaf
            in
                inOrder tree `shouldBe` [m0, m1, m2, m3]

    describe "whatWentWrong" $ do
        it "should return empty messages for empty log" $
            whatWentWrong [] `shouldBe` []

        it "should return empty messages for only unknown" $
            whatWentWrong [(Unknown "la")] `shouldBe` []

        it "should return message for error >= 50 severity" $
            whatWentWrong [(LogMessage (Error 50) 1 "error")] `shouldBe` ["error"]

        it "should not return message for info" $
            whatWentWrong [(LogMessage Info 1 "error")] `shouldBe` []

        it "should return sorted messages for larger example" $
            let
                info1 = LogMessage Info 0 "info 1"
                error1 = LogMessage (Error 1) 1 "error 1"
                info2 = LogMessage Info 2 "info 2"
                error2 = LogMessage (Error 100) 3 "error 2"
                error3 = LogMessage (Error 200) 3 "error 3"
                unknown = Unknown "unknown"
                testMessages = [unknown, info1, error3, unknown, error2, error1, info2]
            in
            whatWentWrong testMessages `shouldBe` ["error 2", "error 3"]