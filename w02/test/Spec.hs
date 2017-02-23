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