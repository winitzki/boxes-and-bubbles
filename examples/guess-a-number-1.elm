module Example exposing (main)

{-| # Overview
A guess-a-number game.

# Running

@docs main

-}

import List exposing (map, foldl)
import Char
import Element exposing (..)
import Color exposing (..)
import Text exposing (fromString)
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Time exposing (Time, now)
import Task exposing (perform)

type Msg = Reset
 | Answer
 | SetGuess Int
 | Ask String


type alias Model = {
    n: Int
    , tries: Int
    , current: String
    , message: String
 }

initModel = { n = 20, tries = 0, current = "", message = "" }

updateAll: Msg -> Model -> (Model, Cmd Msg)
updateAll msg model =
  case msg of
    Reset -> (initModel, Cmd.none)
    Ask x -> ( { model | current = x }, Cmd.none)
    Answer ->
      let
        newMessage = case String.toInt model.current of
           Err msg -> "Error: bad numeric format"
           Ok x ->
             if model.n == x then "you won!" else
             if model.n > x then
              "it is greater than " ++ toString x
             else
              "it is not greater than " ++ toString x
        newModel = { model | tries = model.tries + 1, message = newMessage }
      in
        (newModel, Cmd.none)

    _ -> (model, Cmd.none)

scene : Model -> Html Msg
scene model =
 div []
    [ text "Guess-a-number"
    , div [] []
    , button [ onClick Reset ] [ text "Start over" ]
    , div [] []
    , text "Is the number X greater than "
    , input [ placeholder "your guess", onInput Ask, myStyle ] []
    , text " ? "
    , button [ onClick Answer ] [ text "ask" ]
    , div [ myStyle2 ] [ text model.message ]
    ]

myStyle =
  style
    [ ("width", "80px")
    , ("height", "20px")
    , ("padding", "10px 0")
    , ("font-size", "10px")
    , ("text-align", "center")
    ]

myStyle2 =
  style
    [ ("height", "20px")
    , ("padding", "10px 0")
    , ("font-size", "1em")
    , ("text-align", "left")
    ]

{-| Run the game.
-}

main : Program Never Model Msg
main = Html.program {
  init = (initModel, Cmd.none)
  , update = updateAll
  , subscriptions = always Sub.none
  , view = scene
  }
