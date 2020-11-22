module SolutionsTest exposing (..)


import Expect exposing (Expectation)
import Test exposing (..)
import Solutions exposing (..)


----------------------------------
--wrapper functions
-------------------------------

type SuitWrapper = Diamondsw | Clubsw | Heartsw | Spadesw
type CardWrapper = CardWrapper Face SuitWrapper


suitWrapperToSuit: SuitWrapper -> Suit
suitWrapperToSuit wrapper = 
  case wrapper of
    Diamondsw -> Diamonds
    Clubsw -> Clubs
    Heartsw -> Hearths
    Spadesw -> Spades
suitToSuitWrapper:  Suit -> SuitWrapper
suitToSuitWrapper wrapper = 
  case wrapper of
    Diamonds -> Diamondsw
    Clubs -> Clubsw
    Hearths -> Heartsw
    Spades -> Spadesw


deckWrapper: List CardWrapper
deckWrapper = deck |> List.map(\({suit, value}) -> CardWrapper value (suitToSuitWrapper suit)) --TODO swap Card constructor 

cardValueWrapper : CardWrapper -> List Int 
cardValueWrapper (CardWrapper face suit) = cardValue (Card (suitWrapperToSuit suit) face) ----TODO swap Card constructor
----------------------------------



genTest name elem expected =
  test name <|
    \_ -> Expect.equal expected elem

suite : Test--[Clubs, Diamonds, Hearts, Spades]
suite =
  describe "Solutions tests" [
    describe "deck" [--cam hardcodat, stiu, am vrut sa folosesc functia allDifferent dar trebuia sa fac Card comparable
        genTest "deck 1"  (deckWrapper |> List.length) 52,
        genTest "deck 2"  (deckWrapper |> List.member (CardWrapper Ace Diamondsw)) True,
        genTest "deck 3"  (deckWrapper |> List.member (CardWrapper Ace Clubsw)) True,
        genTest "deck 4"  (deckWrapper |> List.member (CardWrapper Ace Spadesw)) True,
        genTest "deck 5"  (deckWrapper |> List.member (CardWrapper Ace Heartsw)) True,
        genTest "deck 6"  (deckWrapper |> List.member (CardWrapper Six Spadesw)) True,
        genTest "deck 7"  (deckWrapper |> List.member (CardWrapper Seven Spadesw)) True
    ],
    describe "cardValue" [
       genTest "cardvalue 1"  (cardValueWrapper (CardWrapper Ace Diamondsw))   [1, 11],   
       genTest "cardvalue 2"  (cardValueWrapper (CardWrapper Ace Clubsw))      [1, 11],    
       genTest "cardvalue 3"  (cardValueWrapper (CardWrapper Ace Heartsw))     [1, 11],
       genTest "cardvalue 5"  (cardValueWrapper (CardWrapper Ace Spadesw))     [1, 11],
       genTest "cardvalue 6"  (cardValueWrapper (CardWrapper Queen Diamondsw)) [10],
       genTest "cardvalue 7"  (cardValueWrapper (CardWrapper Queen Spadesw))   [10],
       genTest "cardvalue 8"  (cardValueWrapper (CardWrapper Six Diamondsw))   [6],
       genTest "cardvalue 9"  (cardValueWrapper (CardWrapper Six Clubsw))      [6]
    ],
    describe "smallesK" [
      genTest "smallest 1"  (smallestK 10 [1, 5, 2]) [1, 2, 5],
      genTest "smallest 2"  (smallestK 2 [1]) [1],
      genTest "smallest 3"  (smallestK 3 [1, 2]) [1, 2],
      genTest "smallest 4"  (smallestK 3 [5, 3, 8, 1, 12, 2]) [1, 2, 3],
      genTest "smallest 5"  (smallestK 2 [125, 12, 5, 6, 1, 4, 2, 65, 5]) [1, 2],
      genTest "smallest 6"  (smallestK 2 [2, 2, 1, 1, 1]) [1, 1],
      genTest "smallest 7"  (smallestK 0 [2, 2, 1, 1, 1]) []
    ],
    describe "balanced" [
      genTest "balanced 1" (balanced "(if (zero? x) max (/ 1 x))") True,
      genTest "balanced 2" (balanced "I told him (that it’s not (yet) done). (But he wasn’t listening)") True,
      genTest "balanced 3" (balanced ":-)") False,
      genTest "balanced 4" (balanced "())(") False,
      genTest "balanced 5" (balanced "()((((()))") False,
      genTest "balanced 6" (balanced "((((((((()))))))))") True,
      genTest "balanced 7" (balanced ")((((((((()))))))))(") False 
    ],
    describe "coinChange" [
      genTest "coinChange 1" (coinChange 100 [1, 5, 10]) 121,
      genTest "coinChange 2" (coinChange 50 [1, 2, 4, 8]) 504,
      genTest "coinChange 3" (coinChange 25 [1, 2, 5]) 42,
      genTest "coinChange 4" (coinChange 11 [1, 5, 10]) 4,
      genTest "coinChange 5" (coinChange 4 [1, 2]) 3,
      genTest "coinChange 6" (coinChange 1 [1]) 1,
      genTest "coinChange 7" (coinChange 0 [1, 2, 3]) 1,
      genTest "coinChange 8" (coinChange 1 [1, 2, 3]) 1,
      genTest "coinChange 9" (coinChange 1 [1, 2]) 1
    ]
  ]
 