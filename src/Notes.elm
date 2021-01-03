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
    [ staffLine (String.fromFloat (yPos + 0))
    , staffLine (String.fromFloat (yPos + 20))
    , staffLine (String.fromFloat (yPos + 40))
    , staffLine (String.fromFloat (yPos + 60))
    , staffLine (String.fromFloat (yPos + 80))
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
        [ x (String.fromFloat xPos)
        , y (String.fromFloat yPos)
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


getNote : String -> String
getNote key =
    case key of
        "w" ->
            "W"

        "f" ->
            "H"

        "q" ->
            "Q"

        "e" ->
            "E"

        "s" ->
            "S"

        _ ->
            ""


addNotes : List ( String, ( Float, Float ) ) -> String -> ( Float, Float ) -> List ( String, ( Float, Float ) )
addNotes notes note pos =
    case note of
        "W" ->
            Tuple.pair note pos :: notes

        "H" ->
            Tuple.pair note pos :: notes

        "Q" ->
            Tuple.pair note pos :: notes

        "E" ->
            Tuple.pair note pos :: notes

        "S" ->
            Tuple.pair note pos :: notes

        _ ->
            notes


draw notes =
    staffCanvas notes
