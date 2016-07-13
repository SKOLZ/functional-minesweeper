module Tile where

import Html exposing (..)
import Html.Attributes exposing (..)

type Action = Clear | Mark

-- model

type alias Tile = {
  id : Int,
  isMine : Bool,
  isCleared : Bool,
  isMarked : Bool
}

create : Int -> Tile
create id = Tile id False False False

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
      if tile.isMine then
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
  { tile | isMine = True }
---- view

--view : Model -> Html
--view  model =
--  if (model.isCleared && model.isMine) then
--    div [ class "tile cleared mine" ] []
--  else if (model.isCleared && not model.isMine) then
--    div [ class "tile cleared" ] []
--  else if model.isMarked then
--    div [ class "tile marked" ] []
--  else
--    div [ class "tile"] []

---- update

--update : Action -> Model -> Model
--update action model =
--  case action of
--    Clear ->
--      clear model
--    Mark ->
--      mark model

-- other

--init : Bool -> Int ->Model
--init isMine id = {
--  id = id,
--  isMine = isMine,
--  isCleared = False,
--  isMarked = False
-- }



