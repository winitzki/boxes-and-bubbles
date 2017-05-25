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
import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (generate, int)
import Time exposing (Time, now, second)
import Process exposing (sleep)
import Task exposing (perform)

type Msg = Reset
 | Request
 | Answer (String, Int)
 | Ask String
 | SetSecret Int

upperLimit = 50

type alias Model = {
    n: Int
    , tries: Int
    , current: String
    , messages: Array String
    , session: Int
 }

initModel : Model
initModel = { n = 20, tries = 0, current = "", messages = Array.empty, session = 0 }

initFill = Random.generate SetSecret (Random.int 1 upperLimit)

updateAll: Msg -> Model -> (Model, Cmd Msg)
updateAll msg model =
  case msg of
    Reset -> ({ initModel | session = model.session + 1 }, initFill)
    SetSecret x -> ( { model | n = x }, Cmd.none)
    Ask x -> ( { model | current = x }, Cmd.none)
    Request -> (model, perform (\_ -> Answer (model.current, model.session)) (sleep (10 * second)) )
    Answer (current, session) ->
      if session == model.session then
          let
            newMessage = case String.toInt current of
               Err msg -> "Error: bad numeric format"
               Ok x ->
                 if model.n == x then "you got it, the answer is " ++ toString x
                 else if model.n > x then
                  "it is greater than " ++ toString x
                 else
                  "it is not greater than " ++ toString x
            newModel = { model | tries = model.tries + 1, messages = push newMessage model.messages }
          in
            (newModel, Cmd.none)

      else (model, Cmd.none)

--    _ -> (model, Cmd.none)

scene : Model -> Html Msg
scene model =
 div []
    [ text <| "Guess-a-number: 1 to " ++ toString upperLimit
    , div [] []
    , button [ onClick Reset ] [ text "Start over" ]
    , div [] []
    , text "Is the number X greater than "
    , input [ placeholder "your guess", onInput Ask, myStyle ] []
    , text " "
    , button [ onClick Request ] [ text "?" ]
    , div [] []
    , text ("You made " ++ toString model.tries ++ " tries")
    , div [ myStyle2 ] (Array.toList (Array.map (\m -> p [] [text m]) model.messages))
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
  init = (initModel, initFill)
  , update = updateAll
  , subscriptions = always Sub.none
  , view = scene
  }
