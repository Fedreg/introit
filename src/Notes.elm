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
            "160"

        "H" ->
            "80"

        "Q" ->
            "40"

        "E" ->
            "20"

        "S" ->
            "10"

        _ ->
            "0"


noteColor noteName =
    case noteName of
        "W" ->
            "#6AA4B0"

        "H" ->
            "#62CCC0"

        "Q" ->
            "#2B4560"

        "E" ->
            "#E34234"

        "S" ->
            "#00E0EA"

        _ ->
            ""


noteSvg note =
    let
        name =
            Tuple.first note

        nWidth =
            noteWidth name

        fillColor =
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
        , rx "10"
        , ry "10"
        , width nWidth
        , height "20"
        , fill fillColor
        , Svg.Attributes.filter "drop-shadow(1px 1px 2px #aaa)"
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
