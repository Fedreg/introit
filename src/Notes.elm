module Notes exposing (..)

import Model exposing (Note)
import Svg exposing (..)
import Svg.Attributes exposing (..)



--staffCanvas : Svg Msg
--staffCanvas : List


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


noteWidth : String -> String
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


noteWidthFloat : String -> Float
noteWidthFloat noteName =
    Maybe.withDefault 160.0 <| String.toFloat <| noteWidth noteName


noteColor : String -> String
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
        duration =
            note.duration

        nWidth =
            noteWidth duration

        fillColor =
            noteColor duration

        xPos =
            note.x

        yPos =
            negate note.y
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


getNoteDuration : String -> String
getNoteDuration key =
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


addNotes : List Note -> Note -> List Note
addNotes notes note =
    case note.duration of
        "W" ->
            note :: notes

        "H" ->
            note :: notes

        "Q" ->
            note :: notes

        "E" ->
            note :: notes

        "S" ->
            note :: notes

        _ ->
            notes


buildNote : String -> ( Float, Float ) -> Note
buildNote noteDuration pos =
    let
        noteName =
            "C"

        noteOctave =
            4
    in
    case noteDuration of
        "W" ->
            Note (Tuple.first pos) (Tuple.second pos) noteName noteOctave noteDuration

        "H" ->
            Note (Tuple.first pos) (Tuple.second pos) noteName noteOctave noteDuration

        "Q" ->
            Note (Tuple.first pos) (Tuple.second pos) noteName noteOctave noteDuration

        "E" ->
            Note (Tuple.first pos) (Tuple.second pos) noteName noteOctave noteDuration

        "S" ->
            Note (Tuple.first pos) (Tuple.second pos) noteName noteOctave noteDuration

        _ ->
            Note (Tuple.first pos) (Tuple.second pos) "" 0 ""


draw notes =
    staffCanvas notes
