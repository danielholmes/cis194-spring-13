import Test.Hspec
import Homework1
import Homework2
import Homework3
import Homework4

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

    describe "foldTree" $ do
        it "returns Leaf for empty" $
            let result = foldTree []
            in case result of
                Leaf -> True `shouldBe` True
                _ -> False `shouldBe` True

        it "returns single correctly" $
            foldTree "a" `shouldBe` Node 0 Leaf 'a' Leaf

        {-it "returns double correctly" $
            foldTree "ab" `shouldBe` Node 1 Leaf 'b' (Node 0 Leaf 'a' Leaf)

        it "returns example correctly" $
            let
                expected = Node 3
                              (Node 2
                                (Node 0 Leaf 'F' Leaf)
                                'I'
                                (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
                              'J'
                              (Node 2
                                (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
                                'H'
                                (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
            in foldTree "ABCDEFGHIJ" `shouldBe` expected-}

    describe "myFoldl" $ do
        {-it "returns correct for empty" $
            let result = myFoldl (\_ x -> x) 1 []
            in length result `shouldBe` 0-}
        it "returns correct for single" $
            myFoldl (+) 1 [1] `shouldBe` foldl (+) 1 [1]

        it "returns correct for multiple" $
            myFoldl (+) 1 [1..10] `shouldBe` foldl (+) 1 [1..10]

    describe "xor" $ do
        it "returns False for empty" $
            xor [] `shouldBe` False

        it "returns True for single True" $
            xor [True] `shouldBe` True

        it "returns False for single False" $
            xor [False] `shouldBe` False

        it "returns False for double True" $
            xor [True, True] `shouldBe` False

        it "returns correct for example 1" $
            xor [False, True, False] `shouldBe` True

        it "returns correct for example 2" $
            xor [False, True, False, False, True] `shouldBe` False

    describe "map'" $ do
        it "returns empty for empty" $
            let result = map' id []
            in (length result) `shouldBe` 0

        it "returns correct for single" $
            map' (*2) [1] `shouldBe` [2]

        it "returns correct for multiple" $
            map' (*3) [1,10,20] `shouldBe` [3,30,60]

    describe "sieveSundaram" $ do
        it "returns empty for 0" $
            sieveSundaram 0 `shouldBe` []

        it "returns correct for 1" $
            sieveSundaram 1 `shouldBe` [3]

        it "returns correct for 11" $
            sieveSundaram 11 `shouldBe` [3, 5, 7, 11, 13, 17, 19, 23]

