module FunSetTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import FunSet exposing (..)

genTest name set elem expected =
  test name <|
    \_ -> Expect.equal expected (set elem)

suite : Test
suite =
  describe "FunSet tests" [
    describe "union" [
      genTest "union 1" (union (singletonSet 1) (singletonSet 2)) 1 True,
      genTest "union 2" (union (singletonSet 1) (singletonSet 2)) 2 True,
      genTest "union 3" (union (setOf [1, 3, 4]) (setOf [1, 2])) 2 True,
      genTest "union 4" (union (setOf [1, 3, 4]) (setOf [1, 2])) 5 False
    ],
    describe "intersect" [
      genTest "intersect 1" (intersect (setOf [1, 2]) (setOf [1, 3])) 1 True,
      genTest "intersect 2" (intersect (setOf [1, 2]) (setOf [1, 3])) 2 False,
      genTest "intersect 3" (intersect (setOf []) (setOf [1, 3])) 1 False,
      genTest "intersect 4" (intersect (setOf []) (setOf [1, 3])) 2 False,
      genTest "intersect 5" (intersect (setOf []) (setOf [1, 3])) 3 False
    ],
    describe "diff" [
      genTest "diff 1" (diff (setOf [1, 2]) (setOf [1, 3])) 1 False,
      genTest "diff 2" (diff (setOf [1, 2]) (setOf [1, 3])) 2 True,
      genTest "diff 3" (diff (setOf [1, 2, 3, 4]) (setOf [1, 3])) 2 True,
      genTest "diff 4" (diff (setOf [1, 2, 3, 4]) (setOf [1, 3])) 4 True,
      genTest "diff 5" (diff (setOf [1, 2, 3, 4]) (setOf [1, 3])) 1 False,
      genTest "diff 6" (diff (setOf [1, 2, 3, 4]) (setOf [1, 3])) 3 False,
      genTest "diff 7" (diff (setOf [1, 2, 3, 4]) (setOf [])) 3 True,
      genTest "diff 8" (diff (setOf [1, 2, 3, 4]) (setOf [])) 1 True,
      genTest "diff 9" (diff (setOf []) (setOf [1, 2])) 1 False
    ],
    describe "map" [
      genTest "map 1" (map (\x -> x + 1) (setOf [1, 2])) 1 False,
      genTest "map 2" (map (\x -> x + 1) (setOf [1, 2])) 2 True,
      genTest "map 3" (map (\x -> x + 1) (setOf [1, 2])) 3 True,
      genTest "map 4" (map (\x -> x * 2) (setOf [1, 2])) 2 True,
      genTest "map 5" (map (\x -> x * 2) (setOf [1, 2])) 4 True,
      genTest "map 6" (map (\x -> x * 2) (setOf [1, 2])) 1 False,
      genTest "map 7" (map (\x -> x * 2) (setOf [1, 2])) 3 False,
      genTest "map 8" (map (\x -> x * 2) (setOf [1, 2])) 5 False,
      genTest "map 9" (map (\x -> x * 2) (setOf [1, 2])) 6 False
    ],
    describe "fold" [
      genTest "fold 1" (fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 True,
      genTest "fold 2" (fold intersect [setOf [1], setOf [2]]) 1 False,
      genTest "fold 3" (fold intersect [setOf [1], setOf [2]]) 2 False,
      genTest "fold 4" (fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 False,
      genTest "fold 5" (fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 False,
      genTest "fold 6" (fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 True,
      genTest "fold 7" (fold intersect [setOf [1, 2, 3], setOf [1], setOf [2]]) 1 False,
      genTest "fold 8" (fold intersect [setOf [1, 2, 3], setOf [1], setOf [2]]) 2 False,  
      genTest "fold 9" (fold intersect [setOf [1, 2, 3], setOf [1], setOf [2]]) 3 False,
      genTest "fold 10" (fold intersect [setOf [1, 2, 3], setOf [1, 2], setOf [2]] ) 2 True,
      genTest "fold 11" (fold intersect [setOf [1, 2, 3], setOf [1, 2], setOf [2]] ) 1 False,
      genTest "fold 12" (fold intersect [setOf [1, 2, 3], setOf [1, 2], setOf [2]] ) 3 False,
      genTest "fold 13" (fold intersect [setOf [1, 2, 3], setOf [1, 2], setOf [2]] ) 4 False
    ]
  ]