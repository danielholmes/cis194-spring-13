import Test.Hspec
import Employee
import Exercise1
import Exercise2
import Exercise3
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
            (treeFold (\a b -> b) [1] (Node "empty" [])) `shouldBe` [1]

        it "processes non-empty correctly" $
            let
                tree = Node 1
                    [   Node 2 [Node 3 []],
                        Node 10
                        [Node 20 [Node 30 []]]
                    ]
            in
                (treeFold (+) 0 tree) `shouldBe` 66

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

        it "works for best sub division" $
            let
                boss = Emp { empName = "Daniel", empFun = 10 }
                sb1 = Emp { empName = "Minion 1", empFun = 1 }
                sb2 = Emp { empName = "Minion 2", empFun = 5 }
            in
                (nextLevel boss [(GL [sb1] 1, mempty), (GL [sb2] 5, mempty)]) `shouldBe` (GL [boss] 10, GL [sb1, sb2] 6)
