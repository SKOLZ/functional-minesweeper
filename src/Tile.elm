module Tile where

type Action = Clear | Mark

-- model

type alias Model = {
  id : Int
  isMine : Bool,
  isCleared : Bool,
  isMarked : Bool
}

-- update

update : Action -> Model -> Model
update action model =
  case action of
    Clear ->
      clear model
    Mark ->
      mark model

-- view

view : Signal.Address Action -> Model -> Html
view address model =
  if | model.isOpened ->
        div [ class "tile cleared" ] []
     | model.isMarked ->
        div [ class "tile marked",
              onClick address Clear,
              onRightClick address Mark
            ] []
     | otherwise  ->
        div [ class "tile",
              onClick address Clear,
              onRightClick address Mark
            ] []

-- other

init : Bool -> Int ->Model
init isMine id= {
  id = id,
  isMine = isMine,
  isCleared = False,
  isMarked = False
}

makeMine : Model -> Model
makeMine model =
  { model | isMine <- True }


clear : Model -> Model
clear model =
  { model | isCleared <- True }


mark : Model -> Model
mark model =
  if model.isMarked then
    { model | isMarked <- False }
  else
    { model | isMarked <- True }
