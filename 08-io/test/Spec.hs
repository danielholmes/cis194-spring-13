import Test.Hspec
import Employee
import Exercise1
import Exercise2
import Exercise3
import Exercise4
import Data.Tree

main :: IO ()
main = hspec $ do
    describe "glCons" $ do
        it "should add for empty list" $
            let e = Emp { empName = "Daniel", empFun = 5 }
            in (glCons e (GL [] 0)) `shouldBe` (GL [e] 5)

        it "should add for non-empty list" $
            let
                e1 = Emp { empName = "Daniel", empFun = 5 }
                e2 = Emp { empName = "Timothy", empFun = 2 }
            in
                (glCons e2 (GL [e1] 5)) `shouldBe` (GL [e2, e1] 7)

    describe "GuestList Monoid" $ do
        it "has correct mempty" $
            (mempty :: GuestList) `shouldBe` (GL [] 0)

        it "correctly mappends 2 empty" $
            (mappend mempty mempty) `shouldBe` (mempty :: GuestList)

        it "correctly mappends non-empty then empty" $
            let e = Emp { empName = "Daniel", empFun = 5 }
            in (mappend (GL [e] 3) mempty) `shouldBe` (GL [e] 3)

        it "correctly mappends empty then non-empty" $
            let e = Emp { empName = "Daniel", empFun = 5 }
            in (mappend mempty (GL [e] 3)) `shouldBe` (GL [e] 3)

        it "correctly mappends non-empty then non-empty" $
            let
                e1 = Emp { empName = "Daniel", empFun = 5 }
                e2 = Emp { empName = "Timothy", empFun = 2 }
                e3 = Emp { empName = "Lava", empFun = 100 }
            in
                (mappend (GL [e1, e2] 7) (GL [e3] 100)) `shouldBe` (GL [e1, e2, e3] 107)

    describe "moreFun" $ do
        it "returns first correctly" $
            let listOf5 = GL [(Emp { empName = "A", empFun = 5 })] 5
            in (moreFun listOf5 mempty) `shouldBe` listOf5

        it "returns second correctly" $
            let
                listOf5 = GL [(Emp { empName = "A", empFun = 5 })] 5
                listOf10 = GL [(Emp { empName = "B", empFun = 10 })] 10
            in
                (moreFun listOf5 listOf10) `shouldBe` listOf10

    describe "treeFold" $ do
        it "processes empty correctly" $
            (treeFold (\a b -> a + (sum b)) (Node 2 [])) `shouldBe` 2

        it "processes single correctly" $
            (treeFold (\a b -> a + (sum b)) (Node 1 [Node 2 []])) `shouldBe` 3

        it "processes multi-branch correctly" $
            (treeFold (\a b -> a + (sum b)) (Node 1 [Node 10 [], Node 20 []])) `shouldBe` 31

    describe "nextLevel" $ do
        it "works for no sub divisions" $
            let e = Emp { empName = "Daniel", empFun = 10 }
            in (nextLevel e []) `shouldBe` (GL [e] 10, mempty)

        it "works for single sub division" $
            let
                boss = Emp { empName = "Daniel", empFun = 10 }
                subBoss = Emp { empName = "Minion", empFun = 5 }
            in
                (nextLevel boss [(GL [subBoss] 5, mempty)]) `shouldBe` (GL [boss] 10, GL [subBoss] 5)

        it "works for not fun sub divisions" $
            let
                boss = Emp { empName = "Daniel", empFun = 10 }
                sb1 = Emp { empName = "Minion 1", empFun = 1 }
                sb2 = Emp { empName = "Minion 2", empFun = 5 }
            in
                (nextLevel boss [(GL [sb1] 1, mempty), (GL [sb2] 5, mempty)]) `shouldBe` (GL [boss] 10, GL [sb1, sb2] 6)

        it "works for mixture of no boss and sub boss divs" $
            let
                boss = Emp { empName = "Daniel", empFun = 1 }
                notFun = Emp { empName = "Minion 1A", empFun = 10 }
                notFun2 = Emp { empName = "Minion 1B", empFun = 15 }
                fun = Emp { empName = "Minion 2A", empFun = 100 }
                fun2 = Emp { empName = "Minion 2B", empFun = 150 }
                funNotFun = (GL [fun] 100, GL [notFun] 10)
                notFunFun = (GL [notFun2] 15, GL [fun2] 150)
            in
                (nextLevel boss [funNotFun, notFunFun]) `shouldBe` (GL [boss, notFun, fun2] 161, GL [fun, fun2] 250)

    describe "maxFun" $ do
        it "works for given example company" $
            (maxFun testCompany) `shouldBe` (GL [Emp "John" 1, Emp "Sue" 5, Emp "Fred" 3, Emp "Sarah" 17] 26)

        it "works for given example company 2" $
            (maxFun testCompany2) `shouldBe` (GL [Emp "John" 1, Emp "Sue" 5, Emp "Fred" 3, Emp "Sarah" 17] 26)
