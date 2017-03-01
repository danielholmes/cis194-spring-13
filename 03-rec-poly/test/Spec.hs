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

    describe "localMaxima" $ do
        it "returns Empty for Empty" $
            localMaxima [] `shouldBe` []

        it "returns correct for example 1" $
            localMaxima [2,9,5,6,1] `shouldBe` [9,6]

        it "returns correct for example 2" $
            localMaxima [2,3,4,1,5] `shouldBe` [4]

        it "returns correct for example 3" $
            localMaxima [1..5] `shouldBe` []

        it "returns correct for non strictly greater" $
            localMaxima [2,9,5,6,6,1] `shouldBe` [9]

    describe "histogram" $ do
        it "returns Empty for Empty" $
            histogram [] `shouldBe` "==========\n\
                                    \0123456789\n"

        it "returns correct for example 1" $
            histogram [1,1,1,5] `shouldBe` " *        \n\
                                           \ *        \n\
                                           \ *   *    \n\
                                           \==========\n\
                                           \0123456789\n"

        it "returns correct for example 2" $
            histogram  [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "    *     \n\
                                                          \    *     \n\
                                                          \    * *   \n\
                                                          \ ******  *\n\
                                                          \==========\n\
                                                          \0123456789\n"