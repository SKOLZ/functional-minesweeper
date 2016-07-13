module Minefield where

import Tile exposing (..)
import Utils exposing (onRightClick)
import Random exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects)


type Action = Click Tile | Mark Tile

-- model

type alias Model = {
  field: List (List Tile),
  exploded: Bool,
  cleared: Bool
}

-- view

view : Signal.Address Action -> Model -> Html
view address minefield =
  let
    tiles row = List.map (\tile ->
      td [
        class (Tile.tileClass tile),
        onClick address (Click tile),
        onRightClick address (Mark tile)
      ]
      [text ""]
    ) row
    rows = List.map (\row -> tr [] ( tiles row )) minefield.field
  in
    table [] (rows)

-- update

update : Action -> Model -> (Model, Effects Action)
update action minefield =
  case action of
    Click tile ->
      if tile.isMine then
        let
          clearedMinefield = clearAll minefield
        in
          ({clearedMinefield | exploded = True}, Effects.none)
      else
        let
          updatedMinefield = clear tile.id minefield
        in
          ({updatedMinefield | cleared = isCleared updatedMinefield}, Effects.none)

    Mark tile ->
      (mark tile.id minefield, Effects.none)

-- other

create: Int -> Int -> Int -> Model
create width height minesAmount =
  {
    field = initializeMinefield width height minesAmount,
    exploded = False,
    cleared = False
  }

initializeMinefield : Int -> Int -> Int -> List (List Tile)
initializeMinefield width height minesAmount =
  let
    emptyMinefield = makeMinefield width height
  in
    addMines minesAmount (width * height) emptyMinefield

makeMinefield : Int -> Int -> List (List Tile)
makeMinefield width height =
  let
    lowerBound start = start * width + 1
    upperBound start = (start + 1) * width
  in
    List.map (\row -> List.map (\cell -> (Tile.create cell)) [lowerBound row..upperBound row]) [0..height - 1]


-- revisar estoooo!
addMines : Int -> Int -> List (List Tile) -> List (List Tile)
addMines minesAmount range field =
  let
    seed = Random.initialSeed Random.maxInt
    listGenerator = Random.list minesAmount (Random.int 1 range)
    locations = fst (Random.generate listGenerator seed)
    markAsMine tile =
      if List.member tile.id locations then
        Tile.makeMine tile
      else
        tile
  in
    List.map (\row -> List.map markAsMine row) field

isCleared: Model -> Bool
isCleared minefield =
  List.all (\row -> List.all (\tile -> tile.isCleared || tile.isMarked) row) minefield.field

clearAll: Model -> Model
clearAll minefield =
  { minefield | field = List.map (\row -> List.map Tile.clear row) minefield.field }

clear: Int -> Model -> Model
clear tileId minefield =
  {minefield | field = updateTile tileId Tile.clear minefield.field}

mark: Int -> Model -> Model
mark tileId minefield =
  {minefield | field = updateTile tileId Tile.mark minefield.field}

updateTile: Int -> (Tile -> Tile) -> List (List Tile) -> List (List Tile)
updateTile id update field =
  List.map (\row -> List.map (\tile -> if tile.id == id then update tile else tile) row) field
