module Movement exposing (parseMovement)

import Model exposing (Model)


moveConst : Float
moveConst =
    20.0


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


parseMovement : Model -> String -> Model
parseMovement model key =
    let
        dir =
            case key of
                "j" ->
                    "Down"

                "k" ->
                    "Up"

                "h" ->
                    "Left"

                "l" ->
                    "Right"

                _ ->
                    ""

        newMode =
            case key of
                "i" ->
                    "Input"

                "n" ->
                    "Normal"

                _ ->
                    "Normal"
    in
    { model | cursorPos = getNewCursorPos model.cursorPos dir, mode = newMode }
