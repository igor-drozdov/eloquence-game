module PlayingState exposing (..)

import Sentence


type alias State =
    { sentence : Sentence.Model
    , elapsed : Float
    }
