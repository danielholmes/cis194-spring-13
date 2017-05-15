{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Test.Hspec
import JoinList
import Buffer
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

        it "should append when have Empty" $
            ((Single (Sum 3) "a") +++ Empty) `shouldBe` (Single (Sum 3) "a")

        it "should append when have Empty (reverse)" $
            (Empty +++ (Single (Sum 3) "a")) `shouldBe` (Single (Sum 3) "a")

        it "should append with correct monoid" $
            ((Single (Sum 3) "a") +++ (Single (Sum 2) "b")) `shouldBe` (Append (Sum 5) (Single (Sum 3) "a") (Single (Sum 2) "b"))

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
                (dropJ 1 (Append (Size 4) ab cd)) `shouldBe` (Append (Size 3) b cd)

        it "drop all but 1 for multiple return correct" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
                ab = a +++ b
                cd = c +++ d
            in
                (dropJ 3 (Append (Size 4) ab cd)) `shouldBe` d

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
                (takeJ 1 (Append (Size 4) ab cd)) `shouldBe` a

        it "take all but 1 for multiple return correct" $
            let
                a = Single (Size 1) "a"
                b = Single (Size 1) "b"
                c = Single (Size 1) "c"
                d = Single (Size 1) "d"
                ab = a +++ b
                cd = c +++ d
            in
                (takeJ 3 (Append (Size 4) ab cd)) `shouldBe` (Append (Size 3) ab c)

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

    describe "JoinListBuffer toString" $ do
        --it "Empty is correct" $
        --    (toString Empty) `shouldBe` ""

        it "Single is correct" $
            (toString (Single ((Score 2), (Size 1)) "ab")) `shouldBe` "ab"

        it "Append is correct" $
            (toString (Append (Score 4, Size 2) (Single (Score 3, Size 1) "ab") (Single (Score 1, Size 1) "c"))) `shouldBe` "abc"

    describe "JoinListBuffer fromString" $ do
        --it "Empty is correct" $
        --    (fromString "") `shouldBe` Empty

        it "single is correct" $
            (fromString "Dog") `shouldBe` (Single (Score 5, Size 1) "Dog")

        it "multiple is correct" $
            let
                dog = Single (Score 5, Size 1) "Dog"
                rat = Single (Score 3, Size 1) "Rat"
            in
                (fromString "Dog\nRat") `shouldBe` (Append (Score 8, Size 2) dog rat)

    describe "JoinListBuffer line" $ do
        --it "Empty is correct" $
        --    (line 0 Empty) `shouldBe` Nothing

        it "valid single is correct" $
            (line 0 (Single (Score 5, Size 1) "Dog")) `shouldBe` (Just "Dog")

        it "invalid single is correct" $
            (line 1 (Single (Score 5, Size 1) "Dog")) `shouldBe` Nothing

        it "append valid is correct" $
            (line 1 (Append (Score 4, Size 2) (Single (Score 3, Size 1) "ab") (Single (Score 1, Size 1) "c"))) `shouldBe` (Just "c")

        it "append valid is incorrect" $
            (line 2 (Append (Score 4, Size 2) (Single (Score 3, Size 1) "ab") (Single (Score 1, Size 1) "c"))) `shouldBe` Nothing

    describe "JoinListBuffer replaceLine" $ do
        --it "Empty is correct" $
        --    (replaceLine 0 "hello" Empty) `shouldBe` Empty

        it "valid single is correct" $
            (replaceLine 0 "Rat" (Single (Score 5, Size 1) "Dog")) `shouldBe` (Single (Score 3, Size 1) "Rat")

        it "valid append is correct" $
            let
                cat = Single (Score 5, Size 1) "Cat"
                dog = Single (Score 5, Size 1) "Dog"
                rat = Single (Score 3, Size 1) "Rat"
            in
                (replaceLine 1 "Rat" (Append (Score 10, Size 2) dog cat)) `shouldBe` (Append (Score 8, Size 2) dog rat)

        it "valid append complex is correct" $
            let
                quick = Single (Score 20, Size 1) "Quick"
                brown = Single (Score 10, Size 1) "Brown"
                fox = Single (Score 13, Size 1) "Fox"
                jumps = Single (Score 16, Size 1) "Jumps"
                rat = Single (Score 3, Size 1) "Rat"
                qb = Append (Score 30, Size 2) quick brown
                qbr = qb +++ rat
                qbrj = qbr +++ jumps
                fj = Append (Score 29, Size 2) fox jumps
                rj = Append (Score 19, Size 2) rat jumps
                sentence = Append (Score 59, Size 4) qb fj
            in
                (replaceLine 2 "Rat" sentence) `shouldBe` qbrj

        it "invalid line number" $
            (replaceLine 99 "Rat" (Single (Score 5, Size 1) "Dog")) `shouldBe` (Single (Score 5, Size 1) "Dog")

    describe "JoinListBuffer numLines" $ do
        --it "Empty is correct" $
        --    (numLines Empty) `shouldBe` 0

        it "single is correct" $
            (numLines (Single (Score 5, Size 1) "Dog")) `shouldBe` 1

        it "append is correct" $
            (numLines (Append (Score 4, Size 2) (Single (Score 3, Size 1) "ab") (Single (Score 1, Size 1) "c"))) `shouldBe` 2

    describe "JoinListBuffer value" $ do
        --it "Empty is correct" $
        --    (value Empty) `shouldBe` 0

        it "single is correct" $
            (value (Single (Score 5, Size 1) "Dog")) `shouldBe` 5

        it "append is correct" $
            (value (Append (Score 4, Size 2) (Single (Score 3, Size 1) "ab") (Single (Score 1, Size 1) "c"))) `shouldBe` 4
