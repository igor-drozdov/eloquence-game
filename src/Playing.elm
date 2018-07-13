module Playing exposing (..)

import Model exposing (..)
import Set
import Time exposing (Time, second)
import Array
import Task
import Random
import Svg exposing (svg, circle, path)
import Svg.Attributes exposing (cx, cy, r, fill, d)
import Json.Decode as Json
import PlayingState exposing (State, Sentence)
import Html exposing (Html, text, div, img, button, input, span, p)
import Html.Attributes exposing (src, placeholder, value, id, class)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Process
import GameOverState


period : Float
period =
    20


fieldId : String
fieldId =
    "field"


type Msg
    = UpdateWord String
    | AddWord
    | ClearField
    | EndRound (List String) Int
    | Tick Time


init : Model
init =
    let
        sentence =
            Sentence "The application is"
                "great"
                "Be more expressive"
                (Set.fromList
                    [ "awesome"
                    , "amazing"
                    , "breathtaking"
                    , "exceptional"
                    , "fabulous"
                    , "glorious"
                    , "immaculate"
                    , "impressive"
                    , "incredible"
                    , "majestic"
                    , "magnificent"
                    , "marvelous"
                    , "superb"
                    , "supercalifragilisticexpialidocious"
                    , "unbelievable"
                    , "unthinkable"
                    ]
                )
    in
        Playing (State [] sentence 0 "" False)


update : Msg -> State -> ( Model, Cmd Msg )
update msg state =
    case msg of
        UpdateWord value ->
            Playing { state | word = value }
                ! []

        AddWord ->
            if Set.member state.word state.sentence.synonyms then
                let
                    oldSentence =
                        state.sentence

                    newSentence =
                        { oldSentence | synonyms = Set.remove state.word oldSentence.synonyms }
                in
                    Playing { state | words = state.word :: state.words, sentence = newSentence, word = "" }
                        ! []
            else
                ( Playing { state | wrongWord = True }
                , Process.sleep 200 |> Task.perform (\_ -> ClearField)
                )

        ClearField ->
            Playing { state | word = "", wrongWord = False } ! []

        EndRound words randomNumber ->
            let
                word =
                    Array.get randomNumber (Array.fromList words)
            in
                GameOver (GameOverState.State word (List.length state.words)) ! []

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
        , renderSentence state.sentence
        , div [ class "words" ] [ text (String.join ", " state.words) ]
        , renderWordInput state
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


renderWordInput : State -> Html Msg
renderWordInput state =
    let
        onEnter msg =
            let
                isEnter code =
                    if code == 13 then
                        Json.succeed msg
                    else
                        Json.fail "not ENTER"
            in
                on "keydown" (Json.andThen isEnter keyCode)
    in
        input
            [ placeholder "Enter your word"
            , onInput UpdateWord
            , onEnter AddWord
            , value state.word
            , id fieldId
            , class
                (if state.wrongWord then
                    "wrong"
                 else
                    ""
                )
            ]
            []


renderSentence : Sentence -> Html Msg
renderSentence sentence =
    div [ class "wrapper" ]
        [ div [ class "description" ] [ text sentence.description ]
        , div []
            [ text (sentence.prefix ++ " ")
            , span [ id "sentence-word" ] [ text sentence.word ]
            ]
        ]


subscriptions : State -> Sub Msg
subscriptions state =
    Time.every second Tick
