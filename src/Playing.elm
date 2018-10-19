module Playing exposing (..)

import Dom
import Set
import Task
import Time exposing (Time, second)
import Random
import Svg exposing (svg, circle, path)
import Svg.Attributes exposing (cx, cy, r, fill, d)
import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import Playing.Sentence as Sentence


fieldId : String
fieldId =
    "field"


period : Float
period =
    20


type alias Model =
    { sentence : Sentence.Model
    , elapsed : Float
    }


init : ( Model, Cmd Msg )
init =
    let
        focus =
            Dom.focus fieldId |> Task.attempt (\_ -> NoOp)
    in
        ( Model Sentence.init 0, focus )


type Msg
    = SentenceMsg Sentence.Msg
    | Tick Time
    | EndGame (List String) Int
    | Transition Int (Maybe String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SentenceMsg m ->
            let
                ( subMdl, subMsg ) =
                    Sentence.update m model.sentence
            in
                ( { model | sentence = subMdl }, Cmd.map SentenceMsg subMsg )

        EndGame words index ->
            let
                word =
                    List.head <| List.drop index <| words

                command =
                    Transition (List.length model.sentence.words) word
            in
                ( model, Task.succeed command |> Task.perform identity )

        Tick _ ->
            if model.elapsed == period then
                let
                    words =
                        Set.toList model.sentence.synonyms

                    command =
                        Random.generate (EndGame words) (Random.int 0 (List.length words - 1))
                in
                    ( model, command )
            else
                { model | elapsed = model.elapsed + 1 } ! []

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    div [ id "mainbox" ]
        [ renderTime model.elapsed
        , Html.map SentenceMsg <| Sentence.view model.sentence
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick
