-----------------------
-- Livia Mitrica
-- 11.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Basics as List
import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )

type Msg
  = Draw
  | NewCard Card
  | ToogleDeck
  | ResetGame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( Model (newCard::model.hand) (List.filter (\x -> x/= newCard) model.deck) model.showDeck
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
        Model model.hand model.deck (not model.showDeck)
      , Cmd.none
      )

    ResetGame ->
      (
      startingModel
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
--calculateScore : List Card -> Int
calculateScore cards =
    let
        {----------------------------------------version 1-------------------------------------------------------------
        scoreAces11 : List Card -> List Int -> Int -> (List Int, Int)
        scoreAces11 cardsList scoreNoAce nbOfAces =
            case cardsList of
                [] -> (scoreNoAce, nbOfAces)
                x::xs -> if x.face == Ace then scoreAces11 xs (List.map2 (+) scoreNoAce [11]) (nbOfAces+1)
                            else scoreAces11 xs (List.map2 (+) scoreNoAce (Card.cardValue x)) nbOfAces
        (currentScoreAce11, nbOfACes) = scoreAces11 cards [0] 0
        subtractAces : Int -> Int -> Int
        subtractAces currentScore aces = if aces == 0 then currentScore
                                    else
                                        if currentScore > 21 then subtractAces (currentScore - 10) (aces - 1)
                                        else subtractAces currentScore (aces - 1)
        scoreAcesValue11 = case List.head currentScoreAce11 of
                            Just value -> value
                            Nothing -> 0
        ----------------------------------------version 2---------------------------------------------------------------
         scoreNoAces cardsList scoreNoAce nbOfAces =
                     case cardsList of
                         [] -> (scoreNoAce, nbOfAces)
                         x::xs -> if x.face == Ace then scoreNoAces xs (List.map2 (+) scoreNoAce [0]) (nbOfAces+1)
                                     else scoreNoAces xs (List.map2 (+) scoreNoAce (Card.cardValue x)) nbOfAces
         (currentScoreNoAce, nbOfACes) = scoreNoAces cards [0] 0
         currentScoreNoAceValue = case List.head currentScoreNoAce of
                                        Just value -> value
                                        Nothing -> 0
         listScoreAces = case nbOfACes of
                            1 -> [1,11]
                            2 -> [2,12,22]
                            3 -> [3,13,23,33]
                            4 -> [4,14,24,34,44]
                            _ -> [0]
         listPossibleScores =  List.map (\x -> x+currentScoreNoAceValue) listScoreAces
         finalScore = if List.length ( List.filter(\x -> x<=21) listPossibleScores ) >0 then
                                List.head ( List.reverse ( List.filter(\x -> x<=21) listPossibleScores ) )
                        else List.head listPossibleScores--}
         ----------------------------------------version 3--------------------------------------------------------------
         scoreFunction: List Card -> List Int -> List Int
         scoreFunction cardsList score =
             let
                 makeEqualLength list1 list2 = List.concat( List.repeat (List.length list1) list2 )
             in
               case cardsList of
                   [] -> score
                   x::xs -> if x.face /= Ace then scoreFunction xs (List.map2 (+) score (makeEqualLength score (Card.cardValue x)))
                             else scoreFunction xs ((List.map2 (+) score [1,11]) ++ (List.map2 (+) score [11,1]))
         listPossibleScores = List.sort (scoreFunction cards [0])
         finalScore = if List.length ( List.filter(\x -> x<=21) listPossibleScores ) >0 then
                                         List.head ( List.reverse ( List.filter(\x -> x<=21) listPossibleScores ) )
                                 else List.head listPossibleScores

    in
        case finalScore of --call for version 2&3
            Just value -> value
            Nothing -> 0
        --subtractAces scoreAcesValue11 nbOfACes --call for version 1


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    playerHand =
        model.hand
        |> List.map viewCard
    score = calculateScore model.hand
    deckCards =
        model.deck
        |>List.map cardToUnicode
        |> List.intersperse " "
        |> List.map text
    newCard = case (List.head model.hand) of
                 Just card -> viewCard card
                 Nothing -> div [] [text "You haven't drawn a card yet"]
  in
    div []
      [ div [] [ h1 [] [text appName] ]
        ,button [onClick Draw, disabled (score>=21) ] [text "Draw new card"]
        ,button [onClick ToogleDeck] [text "Toggle deck"]
        ,button [onClick ResetGame] [text "Reset game"]
        ,div [] [newCard]
        ,div [] [text "Deck"]
        , if model.showDeck == True then div [style "font-size" "4em"] deckCards else div [] []
        ,div [] [text "Player hand"]
        ,div [] playerHand
        ,div [] [text "Player score"]
        ,div [] [text (String.fromInt score)]
        ,div [] [h2 [] [ if score == 21 then text "Player wins" else if score >21 then text "Player loses" else text "" ]]
      ]