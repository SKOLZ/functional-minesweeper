module Utils where

import Signal exposing (..)
import Html exposing (Attribute)
import Html.Events exposing (..)
import Json.Decode as Json

customOptions : Html.Events.Options
customOptions =
  { stopPropagation = False,
    preventDefault = True
  }


onRightClick : Signal.Address a -> a -> Attribute
onRightClick address msg =
  Html.Events.onWithOptions "contextmenu" customOptions Json.value (\_ -> Signal.message address msg)
