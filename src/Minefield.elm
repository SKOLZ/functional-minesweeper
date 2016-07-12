module Minefield where

type alias MINEFIELD = List (List Tile.Model)
type Action = Click Tile | Mark Tile

-- model

type alias Model = {
  field: MINEFIELD,
  exploded: Bool,
  cleared: Bool
}

-- update

update : Action -> Model -> (Model, Effects Action)
update action minefield =
  case action of
  Click tile ->
    Tile.update Tile.Action.Clear tile
    if tile.isMine then
      let
        clearedMinefield = clearAll field
      in
        ({ clearedMinefield | exploded = True }, Effects.none)
    else
      let
        updatedMinefield = clear tile.id minefield
      in
        ({ updatedMinefield | cleared = isCleared updatedMinefield }, Effects.none)
  Mark tile ->
    Tile.update Tile.Action.Mark tile
    (mark tile.id minefield, Effects.none)

-- view

view : Signal.Address Action -> Model -> Html
view address minefield =
  let
    tiles row = List.map (\tile ->
      td [
        onClick address (Click tile),
        onRightClick address (Mark tile)
      ]
      [ text (toString tile.id)]
    ) row
    rows = List.map (\row -> tr [] ( tiles row )) minefield.field
  in
    table [] (rows)

-- other

create: Int -> Int -> Int -> Model
create width height minesAmount =
  {
    field = initializeMinefield width height minesAmount,
    exploded = False,
    cleared = False
  }

initializeMinefield : Int -> Int -> Int -> MINEFIELD
initializeMinefield width height minesAmount =
  let
    emptyMinefield = makeMinefield width height
  in
    addMines minesAmount (width * height) emptyMinefield

makeMinefield : Int -> Int -> Model
makeMinefield width height =
  let
    lowerBound start = start * width + 1
    upperBound start = (start + 1) * width
  in
    List.map (\row -> List.map (\cell -> (Tile.init False cell)) [lowerBound row..upperBound row]) [0..height - 1]


-- revisar estoooo!
addMines : Int -> Int -> Model -> Model
addMines minesAmount range minefield =
  let
    seed = Random.initialSeed Random.maxInt
    listGenerator = Random.list minesAmount (Random.int 1 range)
    locations = fst (Random.generate listGenerator seed)
    markAsMine cell =
      if List.member (fst cell) locations then
        (fst cell, Tile.makeMine (snd cell))
      else
        cell
  in
    List.map (\row -> List.map markAsMine row) grid

isCleared: Model -> Bool
isCleared minefield =
  List all (\row -> List.all (\tile -> tile.isCleared || tile.isMarked)) minefield.field

clearAll: Model -> Model
clearAll minefield =
  { minefield | field = List.map (\row -> List.map Tile.clear row) minefield.field }
