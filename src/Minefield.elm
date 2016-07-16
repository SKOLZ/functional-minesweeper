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
  cleared: Bool,
  seed: Seed,
  minesAmount: Int
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
      [Tile.tileContent tile]
    ) row
    rows = List.map (\row -> tr [] ( tiles row )) minefield.field
  in
    table [] (rows)

-- update

update : Action -> Model -> (Model, Effects Action)
update action minefield =
  case action of
    Click tile ->
      if tile.isMarked then
          (minefield, Effects.none)
      else
        if tile.isCleared then
          let
            updatedMinefield = clearClearedTile (neighbors tile.id) minefield
          in
            (updatedMinefield, Effects.none)
        else
          if Tile.isMine tile then
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

create: Int -> Int -> Int -> Seed -> Model
create width height minesAmount seed =
  let
    (newField, newSeed) = initializeMinefield width height minesAmount seed
  in
  {
    field = newField,
    exploded = False,
    cleared = False,
    seed = newSeed,
    minesAmount = minesAmount
  }

initializeMinefield : Int -> Int -> Int -> Seed -> (List (List Tile), Seed)
initializeMinefield width height minesAmount seed =
  let
    emptyMinefield = makeMinefield width height
    (minedMinefield, newSeed) = addMines seed minesAmount width height emptyMinefield
  in
    (calculateNeighbors minedMinefield, newSeed)

makeMinefield : Int -> Int -> List (List Tile)
makeMinefield width height =
  let
    lowerBound start = start * width + 1
    upperBound start = (start + 1) * width
  in
    List.map (\row -> List.map (\col -> (Tile.create (row, col))) [0..width - 1]) [0..height - 1]


addMines : Seed -> Int -> Int -> Int -> List (List Tile) -> (List (List Tile), Seed)
addMines seed minesAmount width height field =
  let
    (minePositions, newSeed) = generateMines seed minesAmount [] width height field
    update tile =
      if List.member tile.id minePositions then
        { tile | content = Mine }
      else
        tile
  in
    (List.map (\row -> List.map update row) field, newSeed)

generateMines : Seed -> Int -> List (Int, Int) -> Int -> Int -> List (List Tile) -> ( List ( Int, Int ), Seed )
generateMines seed minesAmount minePositions width height field =
  if List.length minePositions == minesAmount then
    (minePositions, seed)
  else
    let
      (x, seed1) = Random.generate (Random.int 0 (width - 1)) seed
      (y, seed2) = Random.generate (Random.int 0 (height - 1)) seed1
    in
      case List.member (x, y) minePositions of
        True  -> generateMines seed2 minesAmount minePositions width height field
        False -> generateMines seed2 minesAmount ((x, y) :: minePositions) width height field

isCleared: Model -> Bool
isCleared minefield =
  let
    markedTiles = getMarkedTiles minefield.field
  in
    (List.all (\row -> List.all (\tile -> tile.isCleared || tile.isMarked) row) minefield.field) && markedTiles == minefield.minesAmount

getMarkedTiles : List (List Tile) -> Int
getMarkedTiles field =
  let
    flat = List.concat field
    isMarked tile =
      case tile.isMarked of
        True -> 1
        False -> 0
  in
  List.foldl (+) 0 (List.map isMarked flat)

clearAll: Model -> Model
clearAll minefield =
  { minefield | field = List.map (\row -> List.map Tile.clear row) minefield.field }

clear: (Int, Int) -> Model -> Model
clear tileId minefield =
  { minefield | field = clearTileAndNeighbors [tileId] minefield.field }

mark: (Int, Int) -> Model -> Model
mark tileId minefield =
  let
    updatedMineField = { minefield | field = updateTile tileId Tile.mark minefield.field}
  in
    { updatedMineField | cleared = isCleared updatedMineField }

updateTile: (Int, Int) -> (Tile -> Tile) -> List (List Tile) -> List (List Tile)
updateTile id update field =
  List.map (\row -> List.map (\tile -> if tile.id == id then update tile else tile) row) field

clearTileAndNeighbors : List (Int, Int) -> List (List Tile) -> List (List Tile)
clearTileAndNeighbors tiles field =
  case tiles of
    []          -> field
    (x,y)::rest -> case (getIsTileClearedAndContent (x,y) field) of
        (True, _) ->
          clearTileAndNeighbors rest field
        (False, Mine) ->
          clearTileAndNeighbors rest field
        (False, Neighbors 0) ->
          let
            newList = List.append (neighbors (x,y)) rest
          in
            clearTileAndNeighbors newList (clearTile (x, y) field)
        (False, Neighbors n) ->
          clearTileAndNeighbors rest (clearTile (x, y) field)

getTile : (Int, Int) -> List (List Tile) -> Maybe Tile
getTile (x, y) field =
  let
    flat = List.concat field
    found = List.filter (\tile -> tile.id == (x,y)) flat

  in
    case found of
      []     -> Nothing
      hd::tl -> Just hd

getIsTileClearedAndContent : (Int, Int) -> List (List Tile) -> (Bool, Content)
getIsTileClearedAndContent (x,y) field =
  case getTile (x, y) field of
    Just tile -> (tile.isCleared, tile.content)
    Nothing    -> (True, Neighbors 0)

neighbors : (Int, Int) -> List (Int, Int)
neighbors (x, y) =
    [ (x-1,y-1), (x,y-1), (x+1,y-1),
      (x-1,y),            (x+1,y),
      (x-1,y+1), (x,y+1), (x+1,y+1) ]

calculateNeighbors : List (List Tile) -> List (List Tile)
calculateNeighbors field =
  let
    updateTileNeighbors tile =
      if tile.content == Mine then
        tile
      else
        { tile | content = Neighbors (countMines tile field) }

  in
    List.map (\row -> List.map updateTileNeighbors row) field

countMines : Tile -> List (List Tile) -> Int
countMines tile field =
  let
    mineCount (x,y) =
      if isTileMined (x,y) field then
        1
      else
        0
  in
    List.foldl (+) 0 (List.map mineCount (neighbors tile.id))

isTileMined : (Int, Int) -> List (List Tile) -> Bool
isTileMined (x, y) field =
  case getTile (x, y) field of
    Nothing -> False
    Just t  -> t.content == Mine

clearTile : (Int, Int) -> List (List Tile) -> List (List Tile)
clearTile (x,y) field =
  List.map (\row -> List.map (\tile -> if tile.id == (x,y) then { tile | isCleared = True } else tile) row) field

clearClearedTile : List (Int, Int) -> Model -> Model
clearClearedTile tiles minefield =
  case tiles of
    [] -> { minefield | cleared = isCleared minefield }
    (x,y)::rest ->
      let
        currentTile = getTile (x,y) minefield.field
      in
        case currentTile of
          Just tile ->
            if tile.isMarked then
              clearClearedTile rest minefield
            else
              if Tile.isMine tile then
                let
                  clearedMinefield = clearAll minefield
                in
                  {clearedMinefield | exploded = True}
              else
                let
                  updatedField = clearTileAndNeighbors [(x,y)] minefield.field
                in
                  clearClearedTile rest { minefield | field = updatedField }
          Nothing ->
            clearClearedTile rest minefield
