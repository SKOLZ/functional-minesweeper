module Minesweeper where

import Minefield
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Effects exposing (Effects)

type GameState = LevelSelect | Win | Lose | InGame
type Difficulty = Beginner | Advanced | Expert
type Action = NoOp | Select Difficulty | UpdateMinefield Minefield.Action

-- model

type alias Model = {
  minefield: Maybe Minefield.Model,
  state: GameState
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

    -- minefieldHtml = model.minefield |> Maybe.map (Minefield.view (Signal.forwardTo address UpdateMinefield))
    --minefieldHtml = Maybe.map (Minefield.view (Signal.forwardTo address UpdateMinefield)) model.minefield
    minefieldHtml = Maybe.map (Minefield.view (Signal.forwardTo address UpdateMinefield)) model.minefield

    htmlElements = [Maybe.withDefault controlsHtml minefieldHtml]
  in
    div [] htmlElements

-- update

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)

    Select difficulty ->
      ({ model | minefield = Just (boardFor difficulty), state = InGame }, Effects.none)

    UpdateMinefield action ->
      case model.minefield of
        Just minefield ->
          let
            (minefield, effects) = Minefield.update action minefield

            updatedModel =
              if minefield.exploded then
                {model | minefield = Just minefield, state = Lose}
              else
                if minefield.cleared then
                  {model | minefield = Just minefield, state = Win}
                else
                  {model | minefield = Just minefield}
          in
            (updatedModel, Effects.map UpdateMinefield effects)

        Nothing ->
          (model, Effects.none)

-- other

init: Model
init =
  {
    minefield = Nothing,
    state = LevelSelect
  }

translateDifficulty: String -> Difficulty
translateDifficulty optionValue =
  case optionValue of
    "Beginner" -> Beginner
    "Advanced" -> Advanced
    "Expert" -> Expert
    _ -> Beginner

boardFor: Difficulty -> Minefield.Model
boardFor difficulty =
  case difficulty of
    Beginner ->
      Minefield.create 9 9 10
    Advanced ->
      Minefield.create 16 16 40
    Expert ->
      Minefield.create 22 22 99
