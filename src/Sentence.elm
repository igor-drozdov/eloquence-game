module Sentence exposing (..)

import Set exposing (Set)
import Process
import Task
import Html exposing (Html, text, div, img, button, input, span, p)
import Html.Attributes exposing (src, placeholder, value, id, class)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Json.Decode as Json


fieldId : String
fieldId =
    "field"


type alias Model =
    { prefix : String
    , word : String
    , description : String
    , synonyms : Set String
    , input : String
    , wrongWord : Bool
    , words : List String
    }


type Msg
    = UpdateWord String
    | AddWord
    | ClearField


init : Model
init =
    { prefix = "The application is"
    , word = "great"
    , description = "Be more expressive"
    , input = ""
    , wrongWord = False
    , words = []
    , synonyms =
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
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWord value ->
            { model | input = value }
                ! []

        AddWord ->
            if Set.member model.input model.synonyms then
                { model | words = model.input :: model.words, synonyms = Set.remove model.input model.synonyms, input = "" }
                    ! []
            else
                ( { model | wrongWord = True }
                , Process.sleep 200 |> Task.perform (\_ -> ClearField)
                )

        ClearField ->
            { model | input = "", wrongWord = False } ! []


view : Model -> Html Msg
view model =
    div []
        [ renderSentence model
        , div [ class "words" ] [ text (String.join ", " model.words) ]
        , renderWordInput model
        ]


renderWordInput : Model -> Html Msg
renderWordInput model =
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
            , value model.input
            , id fieldId
            , class
                (if model.wrongWord then
                    "wrong"
                 else
                    ""
                )
            ]
            []


renderSentence : Model -> Html Msg
renderSentence model =
    div [ class "wrapper" ]
        [ div [ class "description" ] [ text model.description ]
        , div []
            [ text (model.prefix ++ " ")
            , span [ id "sentence-word" ] [ text model.word ]
            ]
        ]
