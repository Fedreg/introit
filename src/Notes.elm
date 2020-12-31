module Notes exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)



--staffCanvas : Svg Msg


staffCanvas notes =
    svg
        [ width "100%"
        , height "90%"
        , y "200"
        ]
        (List.concat
            [ staffLineGroup 60
            , staffLineGroup 220
            , staffLineGroup 380
            , staffLineGroup 540
            , List.map noteSvg <| notes
            ]
        )


staffLineGroup yPos =
    [ staffLine (Debug.toString (yPos + 0))
    , staffLine (Debug.toString (yPos + 20))
    , staffLine (Debug.toString (yPos + 40))
    , staffLine (Debug.toString (yPos + 60))
    , staffLine (Debug.toString (yPos + 80))
    ]


noteWidth noteName =
    case noteName of
        "W" ->
            "80"

        "H" ->
            "40"

        "Q" ->
            "20"

        "E" ->
            "10"

        "S" ->
            "5"

        _ ->
            "0"


noteColor noteName =
    case noteName of
        "W" ->
            "#333"

        "H" ->
            "red"

        "Q" ->
            "deepPink"

        "E" ->
            "blue"

        "S" ->
            "magenta"

        _ ->
            ""


noteSvg note =
    let
        name =
            Tuple.first note

        nWidth =
            noteWidth name

        color =
            noteColor name

        pos =
            Tuple.second note

        xPos =
            Tuple.first pos

        yPos =
            negate (Tuple.second pos)
    in
    rect
        [ x (Debug.toString xPos)
        , y (Debug.toString yPos)
        , rx "5"
        , ry "5"
        , width nWidth
        , height "20"
        , fill color
        , stroke "white"
        ]
        []


staffLine yPos =
    rect
        [ x "50"
        , y yPos
        , width "1500"
        , height "1"
        , fill "#777"
        ]
        []


defFlugel notes =
    staffCanvas notes
