import Test.Hspec
--import Control.Exception (evaluate)
import Golf

main :: IO ()
main = hspec $ do
    describe "skips" $ do
        it "returns Empty for Empty" $
            let result = skips []
            in (length result) `shouldBe` 0

        it "returns single element correctly" $
            skips [1] `shouldBe` [[1]]

        it "returns 4 elements correctly" $
            skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

        it "returns 6 elements correctly" $
            skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

        it "returns bool elements correctly" $
            skips [True,False] `shouldBe` [[True,False], [False]]