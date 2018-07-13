module Model exposing (..)

import GameOverState
import PlayingState


type Model
    = Start
    | Playing PlayingState.State
    | GameOver GameOverState.State


init : Model
init =
    Start
