module Minesweeper where

import Minefield
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Effects exposing (Effects)
import Random exposing (..)

type GameState = LevelSelect | Win | Lose | InGame
type Difficulty = Beginner | Advanced | Expert
type Action = NoOp | Select Difficulty | UpdateMinefield Minefield.Action | RestartGame

-- model

type alias Model = {
  minefield: Maybe Minefield.Model,
  state: GameState,
  seed: Seed
}

-- view

view : Address Action -> Model -> Html
view address model =
  let
    controlsHtml = div [class "controls"]
      [
        -- Signal.message address (Select translateDifficulty)
        select [on "change" targetValue (translateDifficulty >> Select >> Signal.message address)]
        [
          option [] [text "Select a difficulty..."],
          option [] [text "Beginner"],
          option [] [text "Advanced"],
          option [] [text "Expert"]
        ]
      ]

    gameStateHtml = div [class "state"] [htmlStateImage model.state]
    restartButtonHtml = button [class "restart-button", onClick address RestartGame] [text "Restart Game"]

    minefieldHtml = Maybe.map (Minefield.view (Signal.forwardTo address UpdateMinefield)) model.minefield

    htmlElements =
      case model.state of
        LevelSelect -> [gameStateHtml, controlsHtml]
        otherwise -> [gameStateHtml, Maybe.withDefault controlsHtml minefieldHtml, restartButtonHtml]
  in
    div [] htmlElements

-- update

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

    Select difficulty ->
      ({ model | minefield = Just (boardFor difficulty model.seed), state = InGame }, Effects.none)

    UpdateMinefield action ->
      case model.minefield of
        Just minefield ->
          let
            (minefield, effects) = Minefield.update action minefield

            updatedModel =
              if minefield.exploded then
                {model | minefield = Just minefield, state = Lose, seed = minefield.seed}
              else
                if minefield.cleared then
                  {model | minefield = Just minefield, state = Win, seed = minefield.seed}
                else
                  {model | minefield = Just minefield}
          in
            (updatedModel, Effects.map UpdateMinefield effects)

        Nothing ->
          (model, Effects.none)

    RestartGame ->
      case model.minefield of
        Just minefield ->
          ({model | state = LevelSelect, minefield = Nothing, seed = minefield.seed}, Effects.none)
        Nothing ->
          (model, Effects.none)

-- other

htmlStateImage : GameState -> Html
htmlStateImage state =
  case state of
    LevelSelect -> img [src "./images/levelSelect.png"] []
    InGame -> img [src "./images/ingame.png"] []
    Win -> img [src "./images/win.png"] []
    Lose ->  img [src "./images/lose.png"] []

init: Int -> Model
init seed =
  {
    minefield = Nothing,
    state = LevelSelect,
    seed = initialSeed seed
  }

translateDifficulty: String -> Difficulty
translateDifficulty optionValue =
  case optionValue of
    "Beginner" -> Beginner
    "Advanced" -> Advanced
    "Expert" -> Expert
    _ -> Beginner

boardFor: Difficulty -> Seed -> Minefield.Model
boardFor difficulty seed =
  case difficulty of
    Beginner ->
      Minefield.create 9 9 10 seed
    Advanced ->
      Minefield.create 16 16 40 seed
    Expert ->
      Minefield.create 22 22 99 seed
