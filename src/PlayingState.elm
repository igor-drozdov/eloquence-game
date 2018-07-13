module PlayingState exposing (..)

import Set exposing (Set)


type alias Sentence =
    { prefix : String
    , word : String
    , description : String
    , synonyms : Set String
    }


type alias State =
    { words : List String
    , sentence : Sentence
    , elapsed : Float
    , word : String
    , wrongWord : Bool
    }
