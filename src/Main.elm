module Main exposing (..)

import Html exposing (Html, text, div, img, button, input, span, p)
import Html.Attributes exposing (src, placeholder, value, id, class)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Json.Decode as Json
import Svg exposing (svg, circle, path)
import Svg.Attributes exposing (cx, cy, r, fill, d)
import Time exposing (Time, second)
import Process
import Task
import Array
import Set
import Random
import Dom


---- CONSTANTS ----


period : Float
period =
    20


fieldId : String
fieldId =
    "field"



---- MODEL ----


type Model
    = StartRound
    | PlayingRound PlayingRoundState
    | GameOver GameOverState


type alias Sentence =
    { prefix : String
    , word : String
    , description : String
    , synonyms : Set.Set String
    }


type alias PlayingRoundState =
    { words : List String
    , sentence : Sentence
    , elapsed : Float
    , word : String
    , wrongWord : Bool
    }


type alias GameOverState =
    { hint : Maybe String
    , score : Int
    }


init : ( Model, Cmd Msg )
init =
    ( StartRound, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | StartGame
    | UpdateWord String
    | AddWord
    | ClearField
    | EndRound (List String) Int
    | Tick Time


updatePlayingRound : Msg -> PlayingRoundState -> ( Model, Cmd Msg )
updatePlayingRound msg state =
    case msg of
        UpdateWord value ->
            PlayingRound { state | word = value }
                ! []

        AddWord ->
            if Set.member state.word state.sentence.synonyms then
                let
                    oldSentence =
                        state.sentence

                    newSentence =
                        { oldSentence | synonyms = Set.remove state.word oldSentence.synonyms }
                in
                    PlayingRound { state | words = state.word :: state.words, sentence = newSentence, word = "" }
                        ! []
            else
                ( PlayingRound { state | wrongWord = True }
                , Process.sleep 200 |> Task.perform (\_ -> ClearField)
                )

        ClearField ->
            PlayingRound { state | word = "", wrongWord = False } ! []

        EndRound words randomNumber ->
            let
                word =
                    Array.get randomNumber (Array.fromList words)
            in
                GameOver (GameOverState word (List.length state.words)) ! []

        Tick _ ->
            if state.elapsed == period then
                let
                    words =
                        Set.toList state.sentence.synonyms

                    command =
                        Random.generate (EndRound words) (Random.int 0 (List.length words - 1))
                in
                    ( PlayingRound state, command )
            else
                PlayingRound { state | elapsed = state.elapsed + 1 } ! []

        _ ->
            PlayingRound state ! []


updateIdleState : Msg -> Model -> ( Model, Cmd Msg )
updateIdleState msg model =
    case msg of
        StartGame ->
            let
                focus =
                    Dom.focus fieldId |> Task.attempt (\_ -> NoOp)

                playingModel =
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
                        PlayingRound (PlayingRoundState [] sentence 0 "" False)
            in
                ( playingModel, focus )

        _ ->
            model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PlayingRound state ->
            updatePlayingRound msg state

        StartRound ->
            updateIdleState msg model

        GameOver state ->
            updateIdleState msg model



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        StartRound ->
            renderStartScreen

        PlayingRound state ->
            renderPlayingScreen state

        GameOver state ->
            renderGameOverScreen state


renderStartScreen : Html Msg
renderStartScreen =
    div [ id "mainbox" ]
        [ div [ class "wrapper" ]
            [ div [] [ text "Eloquence" ]
            , div [ class "description" ]
                [ text "Choose your words and tone more precisely." ]
            ]
        , button [ onClick StartGame ] [ text "Start" ]
        ]


renderPlayingScreen : PlayingRoundState -> Html Msg
renderPlayingScreen state =
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


renderWordInput : PlayingRoundState -> Html Msg
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


renderGameOverScreen : GameOverState -> Html Msg
renderGameOverScreen state =
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
            , button [ onClick StartGame ] [ text "Restart" ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
