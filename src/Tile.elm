module Tile where

import Html exposing (..)
import Html.Attributes exposing (..)

type Action = Clear | Mark
type Content = Mine | Neighbors Int

-- model

type alias Tile = {
  id : (Int, Int),
  content : Content,
  isCleared : Bool,
  isMarked : Bool
}

create : (Int, Int) -> Tile
create id = Tile id (Neighbors 0) False False

clear : Tile -> Tile
clear tile =
  { tile | isCleared = True }

mark : Tile -> Tile
mark tile =
  if tile.isMarked || tile.isCleared then
    { tile | isMarked = False }
  else
    { tile | isMarked = True }

tileClass: Tile -> String
tileClass tile =
  let
    addMarked tile class =
      if tile.isMarked then
        class ++ " marked"
      else
        class

    addMine tile class =
      if isMine tile then
        class ++ " mine"
      else
        class

    addCleared tile class =
      if tile.isCleared then
        class ++ " cleared"
      else
        class
  in
    "tile" |> addMarked tile |> addMine tile |> addCleared tile

makeMine : Tile -> Tile
makeMine tile =
  { tile | content = Mine }

isMine : Tile -> Bool
isMine tile =
  tile.content == Mine

tileContent: Tile -> Html
tileContent tile =
  case (tile.isCleared, tile.isMarked, tile.content) of
      (True, _, Mine) -> img [src "./images/mine.png"] []
      (False, True, _) -> img [src "./images/mark.png"] []
      (_, _, Neighbors 0) -> text ""
      (True, _, Neighbors n) -> span [class ("neighbors-" ++ (toString n))] [text (toString n)]
      otherwise -> text ""

