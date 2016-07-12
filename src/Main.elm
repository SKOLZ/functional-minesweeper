module Main where

import StartApp.Simple exposing (start)
import Minesweeper
import Html

main : Signal Html.Html
main =
  start {
    init = Minesweeper.initial,
    view = Minesweeper.view,
    update = Minesweeper.update
  }
