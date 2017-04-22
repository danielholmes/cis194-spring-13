{-# LANGUAGE FlexibleContexts #-}
import Test.Hspec
import Exercise1
import Exercise2
import Exercise3
import Exercise4
import Exercise5
import Exercise6

main :: IO ()
main = hspec $ do
    describe "fib" $ do
        it "fib 0" $
            (fib 0) `shouldBe` 0

        it "fib 1" $
            (fib 1) `shouldBe` 1

        it "fib 2" $
            (fib 2) `shouldBe` 1

        it "fib 3" $
            (fib 3) `shouldBe` 2

        it "fib 4" $
            (fib 4) `shouldBe` 3

        it "fib 5" $
            (fib 5) `shouldBe` 5

        it "fib 6" $
            (fib 6) `shouldBe` 8

    describe "fibs1" $ do
        it "fibs1 0" $
            (take 0 fibs1) `shouldBe` []

        it "fibs1 1" $
            (take 1 fibs1) `shouldBe` [0]

        it "fibs1 5" $
            (take 5 fibs1) `shouldBe` [0, 1, 1, 2, 3]

    describe "fibs2" $ do
        it "fibs2 0" $
            (take 0 fibs2) `shouldBe` []

        it "fibs2 1" $
            (take 1 fibs2) `shouldBe` [0]

        it "fibs2 5" $
            (take 5 fibs2) `shouldBe` [0, 1, 1, 2, 3]

    describe "streamRepeat" $ do
        it "normal case" $
            take 3 (streamToList (streamRepeat 1)) `shouldBe` [1, 1, 1]

    describe "streamMap" $ do
        it "applies op" $
            take 3 (streamToList (streamMap (+1) (streamRepeat 1))) `shouldBe` [2, 2, 2]

    describe "streamFromSeed" $ do
        it "normal case" $
            take 5 (streamToList (streamFromSeed (*2) 1)) `shouldBe` [1, 2, 4, 8, 16]

    describe "nats" $ do
        it "is correct" $
            take 10 (streamToList nats) `shouldBe` [0..9]

    describe "ruler" $ do
        it "is correct" $
            take 14 (streamToList ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1]

    describe "interleaveStreams" $ do
        it "is correct" $
            take 8 (streamToList (interleaveStreams (streamFromSeed (*2) 1) (streamFromSeed succ 10))) `shouldBe` [1, 10, 2, 11, 4, 12, 8, 13]

    describe "Stream Integer" $ do
        it "fromInteger" $
            take 4 (streamToList ((fromInteger 1)::(Stream Integer))) `shouldBe` [1, 0, 0, 0]

        it "negate" $
            take 4 (streamToList (negate (streamRepeat (1::Integer)))) `shouldBe` [(-1), (-1), (-1), (-1)]

        it "+" $
            take 4 (streamToList (((streamRepeat 2)::(Stream Integer)) + ((streamRepeat 5)::(Stream Integer)))) `shouldBe` [7, 7, 7, 7]

        it "*" $
            take 4 (streamToList (((streamFromSeed succ 1)::(Stream Integer)) * ((streamRepeat 2)::(Stream Integer)))) `shouldBe` [2, 6, 8, 10]

        it "/" $
            take 4 (streamToList (((streamFromSeed (+4) 2)::(Stream Integer)) / ((streamRepeat 2)::(Stream Integer)))) `shouldBe` [1, 2, 2, 4]

    describe "fibs3" $ do
        it "correct" $
            take 9 (streamToList fibs3) `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21]
