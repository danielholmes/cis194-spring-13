{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Test.Hspec
import JoinList
import Sized
import Exercise1
import Exercise2
import Exercise3
import Exercise4

newtype Sum a = Sum a
    deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
    mempty  = Sum 0
    mappend = (+)


main :: IO ()
main = hspec $ do
    describe "tag" $ do
        --it "Empty" $
        --    (tag Empty::(JoinList (Monoid (Sum Num)) String)) `shouldBe` 0

        it "should work for Single" $
            (tag (Single (Sum 2) "a")) `shouldBe` 2

        it "should work for Append" $
            (tag (Append (Sum 5) (Single (Sum 2) "a") (Single (Sum 3) "b"))) `shouldBe` 5

    describe "+++" $ do
        --it "should work for Empties" $
        --    (tag (Single (Sum 2) "a")) `shouldBe` 2

        it "should append with correct monoid" $
            ((Single (Sum 3) "a") +++ (Single (Sum 2) "b")) `shouldBe` (Append (Sum 5) (Single (Sum 3) "a") (Single (Sum 2) "b"))

        it "should append with correct monoid including Empty" $
            ((Single (Sum 3) "a") +++ Empty) `shouldBe` (Append (Sum 3) (Single (Sum 3) "a") Empty)

    describe "indexJ" $ do
        --it "should return Nothing for empty" $
        --    (indexJ 0 Empty) `shouldBe` Nothing

        it "should return Nothing for larger index" $
            (indexJ 2 (Single (Size 1) "a")) `shouldBe` Nothing

        it "should return correct for single index" $
            (indexJ 0 (Single (Size 1) "a")) `shouldBe` (Just "a")

        it "should return correct for valid first index" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
            in
                (indexJ 0 (Append (Size 4) (Append (Size 2) a b) (Append (Size 2) c d))) `shouldBe` (Just "a")

        it "should return correct for valid 2nd index" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
            in
                (indexJ 1 (Append (Size 4) (Append (Size 2) a b) (Append (Size 2) c d))) `shouldBe` (Just "b")

        it "should return correct for valid last index" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
            in
                (indexJ 3 (Append (Size 4) (Append (Size 2) a b) (Append (Size 2) c d))) `shouldBe` (Just "d")

    describe "dropJ" $ do
        it "drop 0 return same" $
            (dropJ 0 (Single (Size 1) "a")) `shouldBe` (Single (Size 1) "a")

        it "drop 1 for single return empty" $
            (dropJ 1 (Single (Size 1) "a")) `shouldBe` Empty

        it "drop 1 for multiple return correct" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
                ab = a +++ b
                cd = c +++ d
            in
                (dropJ 1 (Append (Size 4) ab cd)) `shouldBe` (Append (Size 3) (Append (Size 1) Empty b) cd)

        it "drop all but 1 for multiple return correct" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
                ab = a +++ b
                cd = c +++ d
            in
                (dropJ 3 (Append (Size 4) ab cd)) `shouldBe` (Append (Size 1) Empty d)

    describe "takeJ" $ do
        it "take 0 return empty" $
            (takeJ 0 (Single (Size 1) "a")) `shouldBe` Empty

        it "take 1 for single return single" $
            let a = Single (Size 1) "a"
            in (takeJ 1 a) `shouldBe` a

        --it "take from empty returns empty" $
        --    (takeJ 1 Empty) `shouldBe` Empty

        it "take 1 for multiple return correct" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
                ab = a +++ b
                cd = c +++ d
            in
                (takeJ 1 (Append (Size 4) ab cd)) `shouldBe` (Append (Size 1) a Empty)

        it "take all but 1 for multiple return correct" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
                ab = a +++ b
                cd = c +++ d
            in
                (takeJ 3 (Append (Size 4) ab cd)) `shouldBe` (Append (Size 3) ab (Append (Size 1) c Empty))

    describe "score" $ do
        it "1 point chars" $
            (score 'E') `shouldBe` (Score 1)

        it "2 point chars" $
            (score 'D') `shouldBe` (Score 2)

        it "3 point chars" $
            (score 'P') `shouldBe` (Score 3)

        it "4 point chars" $
            (score 'F') `shouldBe` (Score 4)

        it "5 point chars" $
            (score 'K') `shouldBe` (Score 5)

        it "8 point chars" $
            (score 'J') `shouldBe` (Score 8)

    describe "scoreString" $ do
        it "empty is 0" $
            (scoreString "") `shouldBe` (Score 0)

        it "single is single value" $
            (scoreString "G") `shouldBe` (Score 2)

        it "multiple has correct sum" $
            (scoreString "WAVE") `shouldBe` (Score 10)

    describe "scoreLine" $ do
        it "empty is Empty" $
            (scoreLine "") `shouldBe` Empty

        it "single word" $
            (scoreLine "yay") `shouldBe` (Single (Score 9) "yay")

