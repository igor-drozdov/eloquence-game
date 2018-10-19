module Main exposing (..)

import Playing as PlayingWidget
import Start as StartWidget
import GameOver as GameOverWidget
import Html exposing (Html)


init : ( Model, Cmd Msg )
init =
    ( Start, Cmd.none )


type Model
    = Start
    | Playing PlayingWidget.Model
    | GameOver GameOverWidget.Model


type Msg
    = StartMsg StartWidget.Msg
    | PlayingMsg PlayingWidget.Msg
    | GameOverMsg GameOverWidget.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( StartMsg StartWidget.Transition, _ ) ->
            let
                ( newModel, subMsg ) =
                    PlayingWidget.init
            in
                ( Playing newModel, Cmd.map PlayingMsg subMsg )

        ( PlayingMsg (PlayingWidget.Transition score hint), _ ) ->
            ( GameOver <| GameOverWidget.init score hint, Cmd.none )

        ( PlayingMsg m, Playing state ) ->
            let
                ( newModel, subMsg ) =
                    PlayingWidget.update m state
            in
                ( Playing newModel, Cmd.map PlayingMsg subMsg )

        ( GameOverMsg GameOverWidget.Transition, _ ) ->
            let
                ( newModel, subMsg ) =
                    PlayingWidget.init
            in
                ( Playing newModel, Cmd.map PlayingMsg subMsg )

        ( GameOverMsg m, GameOver state ) ->
            let
                ( newModel, subMsg ) =
                    GameOverWidget.update m state
            in
                ( GameOver newModel, Cmd.map GameOverMsg subMsg )

        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing state ->
            Sub.map PlayingMsg <| PlayingWidget.subscriptions state

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model of
        Start ->
            Html.map StartMsg <| StartWidget.view

        Playing state ->
            Html.map PlayingMsg <| PlayingWidget.view state

        GameOver state ->
            Html.map GameOverMsg <| GameOverWidget.view state


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
