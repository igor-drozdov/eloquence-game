module Playing.State exposing (..)

import Playing.Sentence as Sentence


type alias State =
    { sentence : Sentence.Model
    , elapsed : Float
    }


init : State
init =
    State Sentence.init 0
