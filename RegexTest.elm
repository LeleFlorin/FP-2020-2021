module RegexTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Regex exposing (..)

genTest name pat input expected =
  test name <|
    \_ -> Expect.equal expected (match pat (String.toList input))

genFnTest name fn input expected =
  test name <|
    \_ -> Expect.equal expected (fn input)

suite : Test
suite =
  describe "Regex test" [
    describe "matchLit" [
      genFnTest "matchLit 1" (matchLit 'a') ['a', 'b', 'b'] (Ok (['a'], ['b', 'b'])),
      genFnTest "matchLit 2" (matchLit 'c') ['a', 'b', 'b'] (Err ['a', 'b', 'b']),
      genFnTest "matchLit 3" (matchLit 'c') [] (Err [])
    ],
    describe "matchSeq" [
      genFnTest "matchSeq 1" (matchSeq (Literal 'a') (Literal 'b')) ['a', 'b', 'c'] (Ok (['a', 'b'], ['c'])),
      genFnTest "matchSeq 2" (matchSeq (Literal 'a') (Literal 'b')) ['a', 'x', 'c'] (Err (['a', 'x', 'c'])),
      genFnTest "matchSeq 3" (matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c')) ['a', 'b', 'c', 'd'] (Ok (['a', 'b', 'c'], ['d']))
    ],
    describe "matchMany" [
      genFnTest "matchMany 1" (matchMany (Literal 'a')) ['a', 'a', 'a'] (Ok (['a', 'a', 'a'], [])),
      genFnTest "matchMany 2" (matchMany (Literal 'b')) ['a', 'a', 'a'] (Ok ([], ['a', 'a', 'a'])),
      genFnTest "matchMany 3" (matchMany (Literal 'b')) ['b', 'b', 'a'] (Ok (['b', 'b'], ['a'])),
      genFnTest "matchMany 4" (matchMany (Seq (Literal 'b') (Literal 'a'))) ['b', 'a', 'b', 'a', 'c'] (Ok (['b', 'a', 'b', 'a'], ['c'])),
      genFnTest "matchMany 5" (matchMany (Seq (Literal 'b') (Literal 'a'))) ['b', 'a', 'c', 'a', 'c'] (Ok (['b', 'a'], ['c', 'a', 'c']))
    ],
    describe "matchOneOf" [
      genFnTest "matchOneOf 1" (matchOneOf (Literal 'a') (Literal 'b')) ['a', 'a', 'a'] (Ok (['a'], ['a', 'a'])),
      genFnTest "matchOneOf 2" (matchOneOf (Literal 'b') (Literal 'a')) ['a', 'a', 'a'] (Ok (['a'], ['a', 'a'])),
      genFnTest "matchOneOf 3" (matchOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))) ['c', 'd', 'a'] (Ok (['c', 'd'], ['a']))
    ],
    describe "Integration" [
      genTest "Literal Ok" (Literal 'a') "a" (Ok (['a'], [])),
      genTest "Literal Fail" (Literal 'b') "a" (Err ['a']),
      genTest "Many Literal Match Ok" (Many (Literal 'a')) "aaab" (Ok (['a', 'a', 'a'], ['b'])),
      genTest "Many Literal Always Ok" (Many (Literal 'a')) "bbbb" (Ok ([], ['b', 'b', 'b', 'b'])),
      genTest "Seq Literal Ok" (Seq (Literal 'a') (Literal 'b')) "abbb" (Ok (['a', 'b'], ['b', 'b'])),
      genTest "Seq Literal Fail" (Seq (Literal 'a') (Literal 'b')) "acbb" (Err ['a', 'c', 'b', 'b']),
      genTest "OneOf Literal P1 Ok" (OneOf (Literal 'a') (Literal 'b')) "abb" (Ok (['a'], ['b', 'b'])),
      genTest "OneOf Literal P2 Ok" (OneOf (Literal 'a') (Literal 'b')) "baa" (Ok (['b'], ['a', 'a'])),
      genTest "OneOf Seq 1 Ok" (OneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))) "aba" (Ok (['a', 'b'], ['a'])),
      genTest "OneOf Seq 2 Ok" (OneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))) "cda" (Ok (['c', 'd'], ['a'])),
      genTest "Complex 1 Ok" (Seq (Seq (Literal 'a') (Literal 'b')) (Many (Literal 'a'))) "abaaa" (Ok (['a', 'b', 'a', 'a', 'a'], [])),
      genTest "Complex 2 Ok" (Many (Seq (Literal 'a') (Literal 'b'))) "ababba" (Ok (['a', 'b', 'a', 'b'], ['b', 'a']))
    ]
  ]