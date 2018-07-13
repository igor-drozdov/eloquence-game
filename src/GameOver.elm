module GameOver exposing (..)

import Playing
import GameOverState exposing (State)
import Model exposing (Model)
import Dom
import Task
import Html exposing (Html, div, button, text, span)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)


fieldId : String
fieldId =
    "field"


type Msg
    = RestartGame
    | NoOp


update : Msg -> State -> ( Model, Cmd Msg )
update msg state =
    case msg of
        RestartGame ->
            let
                focus =
                    Dom.focus fieldId |> Task.attempt (\_ -> NoOp)
            in
                ( Playing.init, focus )

        NoOp ->
            Model.Start ! []


view : State -> Html Msg
view state =
    let
        hint =
            case state.hint of
                Just word ->
                    div [ class "description" ]
                        [ span [] [ text "Hint: " ]
                        , span [ class "words" ] [ text word ]
                        ]

                Nothing ->
                    div [] []
    in
        div [ id "mainbox" ]
            [ div [ class "wrapper" ]
                [ div [] [ text ("Your score is: " ++ (toString state.score)) ]
                , hint
                ]
            , button [ onClick RestartGame ] [ text "Restart" ]
            ]
