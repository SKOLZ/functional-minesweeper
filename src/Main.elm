module Main where

import StartApp
import Minesweeper
import Html
import Effects exposing (Effects)
import Task exposing (Task)
import Html exposing (Html)

app =
  StartApp.start {
    init = init,
    view = Minesweeper.view,
    update = Minesweeper.update, inputs = []
  }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks

port startTime : Int

init : (Minesweeper.Model, Effects Minesweeper.Action)
init = (Minesweeper.init startTime, Effects.none)
