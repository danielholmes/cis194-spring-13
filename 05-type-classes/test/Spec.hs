import Test.Hspec
import ExprT
import Parser
import StackVM
import Exercise1
import Exercise2
import Exercise3
import Exercise4
import Exercise5
import Exercise6

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7

main :: IO ()
main = hspec $ do
    describe "eval" $ do
        it "should return exact ExprT.Lit value" $
            eval (ExprT.Lit 5) `shouldBe` 5

        it "should return correct for addition" $
            eval (ExprT.Add (ExprT.Lit 5) (ExprT.Lit 2)) `shouldBe` 7

        it "should return correct for multiply" $
            eval (ExprT.Mul (ExprT.Lit 3) (ExprT.Lit 8)) `shouldBe` 24

        it "should return correct for example" $
            eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) `shouldBe` 20

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
            in et `shouldBe` ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)

    describe "Expr Integer" $ do
        it "should eval example correctly" $
            testInteger `shouldBe` Just (-7)

    describe "Expr Bool" $ do
        it "should eval example correctly" $
            testBool `shouldBe` Just True

    describe "Expr MinMax" $ do
        it "should eval example correctly" $
            testMM `shouldBe` Just (MinMax 5)

    describe "Expr Mod7" $ do
        it "should eval example correctly" $
            testMod7 `shouldBe` Just (Mod7 0)

    describe "compile" $ do
        it "should compile simple lit correctly" $
            fmap stackVM (compile "1") `shouldBe` Just (Right (IVal 1))

        it "should compile simple addition correctly" $
            fmap stackVM (compile "7+2") `shouldBe` Just (Right (IVal 9))

        it "should compile simple multiplication correctly" $
            fmap stackVM (compile "4*5") `shouldBe` Just (Right (IVal 20))

        it "should compile multiple op correctly" $
            fmap stackVM (compile "1+4*5") `shouldBe` Just (Right (IVal 21))

        it "should compile braced multiple op correctly" $
            fmap stackVM (compile "(1+4)*5") `shouldBe` Just (Right (IVal 25))

    describe "HasVars" $ do
        it "should work with example 1" $
             (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` Just 9

        it "should work with example 2" $
             (withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing

        it "should work with example 3" $
             (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54