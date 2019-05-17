{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "empty" [
            testCase "empty map is empty" $
                let map = empty :: m Int String in
                Map.null map @?= True
        ],

        testGroup "singleton" [
            testCase "singleton return a singleton map" $
                let map = singleton 1 "one" :: m Int String in
                Map.size map @?= 1
        ],

        testGroup "fromList and toAscList" [
            testCase "empty map from empty list" $
                let map = fromList [] :: m Int String in
                Map.null map @?= True,
            
            testCase "correct map from list" $
                let map = fromList [(2, "a"), (1, "b"), (2, "c"), (3, "b")] :: m Int String in do
                Map.size     map @?= 3
                Map.lookup 1 map @?= Just "b"
                Map.lookup 2 map @?= Just "c"
                Map.lookup 3 map @?= Just "b",
            
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "insert" [
            testCase "insert in empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insert 1 "one" map in
                Map.lookup 1 map' @?= Just "one",
            
            testCase "replaces value" $
                let map  = singleton 1 "one" :: m Int String in
                let map' = Map.insert 1 "none" map in
                Map.lookup 1 map' @?= Just "none"
        ],

        testGroup "insertWith" [
            testCase "alters if value exists" $
                let map  = singleton "I" "Nikita" :: m String String in
                let map' = Map.insertWith (++) "I" "am " map in
                Map.lookup "I" map' @?= Just "am Nikita"
        ],

        testGroup "insertWithKey" [
            testCase "alters if value exists" $
                let map  = singleton 228 "228" :: m Int String in
                let map' = Map.insertWithKey (\k new old -> show k ++ new ++ old) 228 "new " map in
                Map.lookup 228 map' @?= Just "228new 228"
        ],

        testGroup "delete" [
            testCase "does nothing if key does not exists" $
                let map  = empty :: m Int Int in
                let map' = Map.delete 238 map in
                Map.null map @?= True,
        
            testCase "deletes an element if key exists" $
                let map  = Map.fromList [(1, "one"), (2, "two")] :: m Int String in
                let map' = Map.delete 1 map in do
                Map.size     map' @?= 1
                Map.lookup 2 map' @?= Just "two"
        ],

        testGroup "adjust" [
            testCase "does nothing if key does not exists" $
                let map  = Map.empty :: m Int Int in
                let map' = Map.adjust (1 +) 322 map in
                Map.null map' @?= True,
            
            testCase "updates the value if key exists" $
                let map  = singleton 322 227 :: m Int Int in
                let map' = Map.adjust (1 +) 322 map in do
                Map.size       map' @?= 1
                Map.lookup 322 map' @?= Just 228
        ],

        testGroup "adjustWithKey" [
            testCase "updates the value if key exists" $
                let map  = singleton 1 "one" :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ " is " ++ x) 1 map in do
                Map.size     map' @?= 1
                Map.lookup 1 map' @?= Just "1 is one"
        ],

        testGroup "update" [
            testCase "does nothing if key does not exists" $
                let map  = Map.empty :: m Int String in
                let map' = Map.update (\x -> if x == "a" then Just "A" else Nothing) 1 map in
                Map.null map' @?= True,

            testCase "changes value if key exists" $
                let map  = singleton 1 "a" :: m Int String in
                let map' = Map.update (\x -> if x == "a" then Just "A" else Nothing) 1 map in do
                Map.size     map' @?= 1
                Map.lookup 1 map' @?= Just "A",
        
            testCase "deletes key if func returns Nothing" $
                let map  = singleton 1 "b" :: m Int String in
                let map' = Map.update (\x -> if x == "a" then Just "A" else Nothing) 1 map in
                Map.null map' @?= True
        ],

        testGroup "updateWithKey" [
            testCase "updates the value if key exists" $
                let map  = singleton 1 "one" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "one" then Just (show k ++ " is one") else Nothing) 1 map in do
                Map.size     map' @?= 1
                Map.lookup 1 map' @?= Just "1 is one"
        ],

        testGroup "alter" [
            testCase "updates value if key exists" $
                let map  = singleton 1 "a" :: m Int String in
                let map' = Map.alter (fmap $ (++) "b") 1 map in do
                Map.size     map' @?= 1
                Map.lookup 1 map' @?= Just "ba"
        ],

        testGroup "lookup" [
            testCase "find element if key exists" $
                let map = singleton 1 "a" :: m Int String in
                Map.lookup 1 map @?= Just "a"
        ],

        testGroup "member" [
            testCase "if key does not exist returns False" $
                let map = empty :: m String Int in
                Map.member "yeputons" map @?= False,
            
            testCase "if key exists returns True" $
                let map = singleton "yeputons" "reads this" :: m String String in
                Map.member "yeputons" map @?= True
        ],

        testGroup "notMember" [
            testCase "if key does not exists returns True" $
                let map = empty :: m Int Int in
                Map.notMember 322 map @?= True,
        
            testCase "if key exists returns False" $
                let map = singleton "solo" 322 :: m String Int in
                Map.notMember "solo" map @?= False
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
