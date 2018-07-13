module Main exposing (..)

import Model exposing (..)
import Playing
import Start
import GameOver
import Html exposing (Html)


init : ( Model, Cmd Msg )
init =
    ( Model.init, Cmd.none )


type Msg
    = StartMsg Start.Msg
    | PlayingMsg Playing.Msg
    | GameOverMsg GameOver.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Playing state ->
            case msg of
                PlayingMsg m ->
                    let
                        ( newModel, subMsg ) =
                            Playing.update m state
                    in
                        ( newModel, Cmd.map PlayingMsg subMsg )

                _ ->
                    model ! []

        Start ->
            case msg of
                StartMsg m ->
                    let
                        ( newModel, subMsg ) =
                            Start.update m
                    in
                        ( newModel, Cmd.map StartMsg subMsg )

                _ ->
                    model ! []

        GameOver state ->
            case msg of
                GameOverMsg m ->
                    let
                        ( newModel, subMsg ) =
                            GameOver.update m state
                    in
                        ( newModel, Cmd.map GameOverMsg subMsg )

                _ ->
                    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing state ->
            Sub.map PlayingMsg <| Playing.subscriptions state

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model of
        Start ->
            Html.map StartMsg <| Start.view

        Playing state ->
            Html.map PlayingMsg <| Playing.view state

        GameOver state ->
            Html.map GameOverMsg <| GameOver.view state


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
