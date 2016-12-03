module Main exposing (..)

import Html exposing (div, button, text)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import Deque


type alias Model =
    { value : Int
    }


example2 =
    Deque.fromList [2..10]
        |> Deque.takeBack 3



-- [ 2, 10, 9, 8 ]
-- Just 2


init =
    { value = 0 } ! []


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.batch [])
        }


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , div [] [ text (toString example2) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            { model | value = model.value + 1 } ! []

        Decrement ->
            { model | value = model.value - 1 } ! []
