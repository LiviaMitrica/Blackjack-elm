-----------------------
-- Livia Mitrica
-- 11.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face =  Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamond | Hearts | Spades
type alias Card = {face: Face, suit:  Suit}

faceToString : Face -> String
faceToString face =
    case face of
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nine"
        Ten -> "Ten"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"

suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs -> "Clubs"
        Diamond -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"

cardToString : Card -> String
cardToString card = faceToString card.face ++ " of " ++ suitToString card.suit

cardValue : Card -> List Int
cardValue card =
    case card.face of
        Ace -> [1, 11]
        Two -> [2]
        Three -> [3]
        Four -> [4]
        Five -> [5]
        Six -> [6]
        Seven -> [7]
        Eight -> [8]
        Nine -> [9]
        _ -> [10]

deck : List Card
deck =
      let
         listFaces = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
         listSuits = [Clubs, Diamond, Hearts, Spades]
         makeSuitFace: List Suit -> Face -> List Card
         makeSuitFace suits face =
                    case suits of
                       []->[]
                       x::xs -> (Card face x)::(makeSuitFace xs face)
         deckHelper l listF =
                    case listF of
                       []->l
                       x::xs -> l ++ (makeSuitFace listSuits x)++deckHelper l xs
      in
         deckHelper [] listFaces

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode card =
    let
        face = card.face
        suit = card.suit
    in
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamond -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamond -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamond ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamond -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamond -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamond -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamond -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamond ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamond ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamond -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamond -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamond -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamond -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card =
   let
     suit = card.suit
     face = card.face
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamond -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "6em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.6em"]  [text (cardToString card)]
     ]