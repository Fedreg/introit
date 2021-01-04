module Notes exposing (..)

import Model exposing (Note)
import Svg exposing (..)
import Svg.Attributes exposing (..)


staffCanvas : List Note -> Svg msg
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


staffLineGroup : Float -> List (Svg msg)
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


noteSvg : Note -> Svg msg
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
        { name, octave } =
            noteNameAndOctaveByPos -150 pos
    in
    case noteDuration of
        "W" ->
            Note (Tuple.first pos) (Tuple.second pos) name octave noteDuration

        "H" ->
            Note (Tuple.first pos) (Tuple.second pos) name octave noteDuration

        "Q" ->
            Note (Tuple.first pos) (Tuple.second pos) name octave noteDuration

        "E" ->
            Note (Tuple.first pos) (Tuple.second pos) name octave noteDuration

        "S" ->
            Note (Tuple.first pos) (Tuple.second pos) name octave noteDuration

        _ ->
            Note (Tuple.first pos) (Tuple.second pos) "" 0 ""



-- Each staff has a "base" value which is the x posiiton of the middle C note
-- We use this to compute the name and octave of the entered note


nameByPos : Int -> String
nameByPos distanceToBase =
    case distanceToBase of
        0 ->
            "C"

        10 ->
            "D"

        20 ->
            "E"

        30 ->
            "F"

        40 ->
            "G"

        50 ->
            "A"

        60 ->
            "B"

        _ ->
            "C"


octaveByPos : Int -> Int
octaveByPos distanceToBase =
    if (distanceToBase >= 70) && (distanceToBase < 130) then
        3

    else if distanceToBase >= 130 then
        4

    else if (distanceToBase >= 0) && (distanceToBase < 70) then
        2

    else if (distanceToBase < 0) && (distanceToBase > -70) then
        1

    else
        0


noteNameAndOctaveByPos base pos =
    let
        xPos =
            round (Tuple.second pos)

        distanceToBase =
            negate (base - xPos)

        octave =
            octaveByPos distanceToBase

        name =
            nameByPos (remainderBy 70 distanceToBase)
    in
    { octave = octave, name = name }


draw : List Note -> Svg msg
draw notes =
    staffCanvas notes
