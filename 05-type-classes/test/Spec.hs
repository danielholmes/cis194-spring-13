import Test.Hspec
import Calc
import ExprT
import Parser

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

{-testInteger = testExp :: Maybe Integer-}
{-testBool = testExp :: Maybe Bool-}
{-testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7-}

main :: IO ()
main = hspec $ do
    describe "eval" $ do
        it "should return exact Lit value" $
            eval (Lit 5) `shouldBe` 5

        it "should return correct for addition" $
            eval (Add (Lit 5) (Lit 2)) `shouldBe` 7

        it "should return correct for multiply" $
            eval (Mul (Lit 3) (Lit 8)) `shouldBe` 24

        it "should return correct for example" $
            eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    describe "evalStr" $ do
        it "should return Nothing for empty" $
            evalStr "" `shouldBe` Nothing

        it "should return Nothing for unbalanced braces" $
            evalStr "2 * (5 * (7" `shouldBe` Nothing

        it "should return correct for literal value" $
            evalStr "1" `shouldBe` Just 1

        it "should return correct for addition" $
            evalStr "1+2" `shouldBe` Just 3

        it "should return correct for multiplication" $
            evalStr "3*4" `shouldBe` Just 12

    describe "ExprT" $ do
        it "should have equivalent Expr value" $
            let et = mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
            in et `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)

    {-describe "evalBool" $ do
        it "should return True for positive Lit value" $
            evalBool (LitB 5) `shouldBe` True

        it "should return False for 0 Lit value" $
            evalBool (LitB 0) `shouldBe` False

        it "should return correct for addition True" $
            evalBool (AddB (LitB 5) (LitB (-2))) `shouldBe` True

        it "should return correct for addition False" $
            evalBool (AddB (LitB (-5)) (LitB (-2))) `shouldBe` False

        it "should return correct for multiply True" $
            evalBool (MulB (LitB 3) (LitB 8)) `shouldBe` True

        it "should return correct for multiply False" $
            evalBool (MulB (LitB 3) (LitB (-8))) `shouldBe` False

    describe "Expr instances" $ do
        it "should have Integer version" $
            testInteger () `shouldBe` Just (-7)

        it "should have Bool version" $
            let testBool = testExp :: Maybe Bool
            in testBool `shouldBe` Just False

        it "should have MinMax version" $
            let testMM = testExp :: Maybe MinMax
            in testMM `shouldBe` 5

        it "should have Mod7 version" $
            let testSat = testExp :: Maybe Mod7
            in testSat `shouldBe` 0 -}