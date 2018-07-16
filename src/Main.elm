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
    case ( msg, model ) of
        ( PlayingMsg m, Playing state ) ->
            let
                ( newModel, subMsg ) =
                    Playing.update m state
            in
                ( newModel, Cmd.map PlayingMsg subMsg )

        ( StartMsg m, Start ) ->
            let
                ( newModel, subMsg ) =
                    Start.update m
            in
                ( newModel, Cmd.map StartMsg subMsg )

        ( GameOverMsg m, GameOver state ) ->
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
