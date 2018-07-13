module Playing exposing (..)

import Model exposing (..)
import Set
import Time exposing (Time, second)
import Array
import Random
import Svg exposing (svg, circle, path)
import Svg.Attributes exposing (cx, cy, r, fill, d)
import Playing.State exposing (State)
import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import GameOver.State as GameOverState
import Playing.Sentence as Sentence


period : Float
period =
    20


type Msg
    = SentenceMsg Sentence.Msg
    | EndRound (List String) Int
    | Tick Time


update : Msg -> State -> ( Model, Cmd Msg )
update msg state =
    case msg of
        SentenceMsg m ->
            let
                ( subMdl, subMsg ) =
                    Sentence.update m state.sentence
            in
                ( Playing { state | sentence = subMdl }, Cmd.map SentenceMsg subMsg )

        EndRound words randomNumber ->
            let
                word =
                    Array.get randomNumber (Array.fromList words)
            in
                GameOver (GameOverState.State word (List.length state.sentence.words)) ! []

        Tick _ ->
            if state.elapsed == period then
                let
                    words =
                        Set.toList state.sentence.synonyms

                    command =
                        Random.generate (EndRound words) (Random.int 0 (List.length words - 1))
                in
                    ( Playing state, command )
            else
                Playing { state | elapsed = state.elapsed + 1 } ! []


view : State -> Html Msg
view state =
    div [ id "mainbox" ]
        [ renderTime state.elapsed
        , Html.map SentenceMsg <| Sentence.view state.sentence
        ]


renderTime : Float -> Html Msg
renderTime elapsed =
    let
        halfOfMinute =
            period / 2

        midAttr =
            20

        radius =
            15

        midAttrStr =
            toString midAttr

        radiusStr =
            toString radius

        angle =
            pi * (elapsed / halfOfMinute) - pi / 2

        arcX =
            midAttr + radius * cos (angle)

        arcY =
            midAttr + radius * sin (angle)

        angleAttr =
            (toString arcX) ++ "," ++ (toString arcY) ++ " z"

        aAttr =
            "A" ++ radiusStr ++ "," ++ radiusStr

        mAttr =
            "M" ++ midAttrStr ++ "," ++ midAttrStr

        lAttr =
            "L" ++ midAttrStr ++ "," ++ (toString (midAttr - radius))

        midCoords =
            midAttrStr ++ "," ++ toString (midAttr + radius)

        controls =
            "1 0,1"

        joined =
            String.join " "

        arcAttr =
            joined [ mAttr, lAttr, aAttr, controls ]

        dAttr =
            if elapsed > halfOfMinute then
                joined [ arcAttr, midCoords, aAttr, controls, angleAttr ]
            else
                joined [ arcAttr, angleAttr ]
    in
        svg []
            [ circle [ cx midAttrStr, cy midAttrStr, r radiusStr ] []
            , path [ d dAttr ] []
            ]


subscriptions : State -> Sub Msg
subscriptions state =
    Time.every second Tick
