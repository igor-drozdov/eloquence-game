module Start exposing (..)

import Model exposing (Model)
import Dom
import Task
import Playing
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)


fieldId : String
fieldId =
    "field"


type Msg
    = StartGame
    | NoOp


update : Msg -> ( Model, Cmd Msg )
update msg =
    case msg of
        StartGame ->
            let
                focus =
                    Dom.focus fieldId |> Task.attempt (\_ -> NoOp)
            in
                ( Playing.init, focus )

        NoOp ->
            Model.Start ! []


view : Html Msg
view =
    div [ id "mainbox" ]
        [ div [ class "wrapper" ]
            [ div [] [ text "Eloquence" ]
            , div [ class "description" ]
                [ text "Choose your words and tone more precisely." ]
            ]
        , button [ onClick StartGame ] [ text "Start" ]
        ]
