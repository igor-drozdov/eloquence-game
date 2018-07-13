module Model exposing (..)

import GameOver.State as GameOverState
import Playing.State as PlayingState


type Model
    = Start
    | Playing PlayingState.State
    | GameOver GameOverState.State


init : Model
init =
    Start
