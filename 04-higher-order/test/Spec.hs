import Test.Hspec
import Homework

main :: IO ()
main = hspec $ do
    describe "fun1" $ do
        it "returns 1 for Empty" $
            fun1 [] `shouldBe` 1

        it "returns -1 for [1]" $
            fun1 [1] `shouldBe` 1

        it "returns -1 for [5,2]" $
            fun1 [5,2] `shouldBe` 0

    describe "fun1'" $ do
        it "returns 1 for Empty" $
            fun1' [] `shouldBe` 1

        it "returns -1 for [1]" $
            fun1' [1] `shouldBe` 1

        it "returns -1 for [5,2]" $
            fun1' [5,2] `shouldBe` 0

    describe "fun2" $ do
        it "returns 0 for 1" $
            fun2 1 `shouldBe` 0

        it "returns 2 for 2" $
            fun2 2 `shouldBe` 2

        it "returns 40 for 3" $
            fun2 3 `shouldBe` 40

        it "returns 6 for 4" $
            fun2 4 `shouldBe` 6

        it "returns 30 for 5" $
            fun2 5 `shouldBe` 30

    describe "fun2'" $ do
        it "returns 0 for 1" $
            fun2' 1 `shouldBe` 0

        it "returns 2 for 2" $
            fun2' 2 `shouldBe` 2

        it "returns 40 for 3" $
            fun2' 3 `shouldBe` 40

        it "returns 6 for 4" $
            fun2' 4 `shouldBe` 6

        it "returns 30 for 5" $
            fun2' 5 `shouldBe` 30