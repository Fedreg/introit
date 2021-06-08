module Cursor exposing (cursor, getNewCursorPos)

import Html exposing (..)
import Html.Attributes exposing (style)


moveConst : Float
moveConst =
    10.0


cursor : ( Float, Float ) -> Html msg
cursor pos =
    let
        x =
            Tuple.first pos

        y =
            negate (Tuple.second pos)
    in
    div
        [ style "background-color" "red"
        , style "height" "20px"
        , style "width" "10px"
        , style "position" "fixed"
        , style "top" (String.fromFloat y ++ "px")
        , style "left" (String.fromFloat x ++ "px")
        ]
        []


getNewCursorPos : ( Float, Float ) -> String -> ( Float, Float )
getNewCursorPos currentPos dir =
    let
        x =
            Tuple.first currentPos

        y =
            Tuple.second currentPos
    in
    case dir of
        "Left" ->
            ( Basics.max (x - moveConst) 0, y )

        "Right" ->
            ( x + moveConst, y )

        "Up" ->
            ( x, Basics.min (y + moveConst) 0 )

        "Down" ->
            ( x, y - moveConst )

        _ ->
            ( x, y )
