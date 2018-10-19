module Start exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)


type Msg
    = Transition


view : Html Msg
view =
    div [ id "mainbox" ]
        [ div [ class "wrapper" ]
            [ div [] [ text "Eloquence" ]
            , div [ class "description" ]
                [ text "Choose your words and tone more precisely." ]
            ]
        , button [ onClick Transition ] [ text "Start" ]
        ]
