module GameOver exposing (..)

import Task
import Html exposing (Html, div, button, text, span)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)


fieldId : String
fieldId =
    "field"


init : Int -> Maybe String -> Model
init =
    Model


type alias Model =
    { score : Int
    , hint : Maybe String
    }


type Msg
    = Transition
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            ( model, Task.succeed Transition |> Task.perform identity )

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    let
        hint =
            case model.hint of
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
                [ div [] [ text ("Your score is: " ++ (toString model.score)) ]
                , hint
                ]
            , button [ onClick Restart ] [ text "Restart" ]
            ]
