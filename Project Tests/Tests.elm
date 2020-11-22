module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Main exposing (..)
import Card exposing (..)

genTest name fn input expected =
  test name <|
    \_ -> Expect.equal expected (fn input)

{-
  Uncomment the tests after you complete the `Card` module and implement the `calculateScore` function
-}
suite : Test
suite = describe "Test" [
  describe "calculateScore"
    [  genTest "calculateScore 1" calculateScore [Card King Hearts] 10
    ,  genTest "calculateScore 2" calculateScore [Card Ace Spades, Card Nine Diamonds, Card Two Spades] 12
    ,  genTest "calculateScore 3" calculateScore [Card Two Hearts, Card King Spades] 12
    ,  genTest "calculateScore 4" calculateScore [Card Ace Hearts, Card King Spades] 21
    ,  genTest "calculateScore 5" calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] 13
    ,  genTest "calculateScore 6" calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] 22
    ,  genTest "calculateScore 7" calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] 21
    ,  genTest "calculateScore 8" calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] 22
    ,  genTest "calculateScore 9" calculateScore [Card Ace Spades, Card Ace Clubs, Card Ace Diamonds, Card Ace Hearts] 14
    ,  genTest "calculateScore 10" calculateScore [Card Jack Spades, Card Three Clubs, Card Three Diamonds, Card Five Clubs] 21
    ,  genTest "calculateScore 11" calculateScore [Card Ace Spades, Card Ace Clubs] 12
    ,  genTest "calculateScore 12" calculateScore [Card Ace Spades, Card Six Clubs, Card Three Hearts, Card King Clubs] 20
    ,  genTest "calculateScore 13" calculateScore [Card Ace Spades, Card Six Clubs, Card Three Hearts, Card King Clubs, Card Six Diamonds] 26
    ,  genTest "calculateScore 14" calculateScore [Card Three Spades, Card Two Clubs, Card Queen Clubs, Card Four Clubs, Card Ace Diamonds] 20
    ,  genTest "calculateScore 15" calculateScore [Card Three Spades, Card Two Clubs, Card Queen Clubs, Card Four Clubs, Card Ace Diamonds, Card Seven Diamonds] 27
    ,  genTest "calculateScore 16" calculateScore [Card Queen Spades, Card King Clubs, Card Ace Clubs] 21
    ,  genTest "calculateScore 17" calculateScore [Card Nine Spades, Card King Clubs, Card Ace Clubs] 20
    ,  genTest "calculateScore 18" calculateScore [Card Nine Spades, Card King Clubs, Card Ace Clubs, Card Ace Diamonds] 21
    ,  genTest "calculateScore 19" calculateScore [Card Ace Spades, Card Six Clubs, Card Three Hearts, Card King Clubs, Card Ten Diamonds] 30
    ,  genTest "calculateScore 20" calculateScore [Card Ace Spades, Card Ace Clubs, Card Ace Diamonds, Card Ace Hearts, Card Ten Diamonds] 14
    ,  genTest "calculateScore 21" calculateScore [Card Ace Spades, Card Nine Diamonds] 20
    ]
  ]

