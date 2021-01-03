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
        dur =
            note.duration

        nWidth =
            noteWidth dur

        fillColor =
            noteColor dur
    in
    rect
        [ x (Debug.toString note.x)
        , y (Debug.toString (negate note.y))
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
        , id yPos
        ]
        []



-- Determines how many px from the base of each staff a note is
-- Each note step is 10; octave is 70


distanceToBase base note octave =
    let
        octaveMod =
            case octave of
                "1" ->
                    -140

                "2" ->
                    -70

                "3" ->
                    0

                "4" ->
                    70

                "5" ->
                    140

                _ ->
                    0

        noteMod =
            case note of
                "A" ->
                    -20

                "B" ->
                    -10

                "C" ->
                    0

                "D" ->
                    10

                "E" ->
                    20

                "F" ->
                    30

                "G" ->
                    40

                _ ->
                    0

        a =
            Debug.log "NoteDistance" [ octaveMod, noteMod ]
    in
    base - negate (octaveMod + noteMod)


draw notes =
    staffCanvas notes
