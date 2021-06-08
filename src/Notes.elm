module Notes exposing (..)

import Model exposing (Note)
import Svg exposing (..)
import Svg.Attributes exposing (..)


measureWidth : Int
measureWidth =
    160


staffLine : Int -> Int -> Svg msg
staffLine xPos yPos =
    rect
        [ x (String.fromInt xPos)
        , y (String.fromInt yPos)
        , width (String.fromInt measureWidth)
        , height "1"
        , fill "#777"
        ]
        []


ledgerLine : Svg msg
ledgerLine =
    staffLine 10


barLine : Int -> Int -> Svg msg
barLine xPos yPos =
    rect
        [ x (String.fromInt xPos)
        , y (String.fromInt yPos)
        , width "1"
        , height "80"
        , fill "#777"
        ]
        []


measure : ( Int, Int ) -> List (Svg msg)
measure xy =
    let
        xPos =
            Tuple.first xy

        yPos =
            Tuple.second xy
    in
    [ barLine xPos yPos
    , staffLine xPos (yPos + 0)
    , staffLine xPos (yPos + 20)
    , staffLine xPos (yPos + 40)
    , staffLine xPos (yPos + 60)
    , staffLine xPos (yPos + 80)
    , barLine (xPos + measureWidth) yPos
    ]


measures : Int -> Int -> List (Svg msg)
measures yPos count =
    let
        ranges =
            List.map
                (\n -> 50 + 160 * n)
                (List.range 0 count)
    in
    List.concatMap
        (\a -> measure (Tuple.pair a yPos))
        ranges


staffCanvas : Int -> List Note -> Int -> String -> Svg msg
staffCanvas measureCount notes yPos sid =
    svg
        [ width "100%"
        , height "300px"
        , y (String.fromInt yPos)
        , id sid
        ]
        (List.concat
            [ measures 70 measureCount
            , List.map noteSvg <| notes
            ]
        )


noteWidth : String -> String
noteWidth noteDuration =
    case String.toLower noteDuration of
        "w" ->
            "160"

        "f" ->
            "80"

        "q" ->
            "40"

        "e" ->
            "20"

        "s" ->
            "10"

        "t" ->
            "5"

        _ ->
            "0"


noteWidthFloat : String -> Float
noteWidthFloat noteDuration =
    Maybe.withDefault 160.0 <| String.toFloat <| noteWidth noteDuration


noteColor : String -> String
noteColor noteDuration =
    case String.toLower noteDuration of
        "w" ->
            "#6AA4B0"

        "f" ->
            "#62CCC0"

        "q" ->
            "#2B4560"

        "e" ->
            "#E34234"

        "s" ->
            "#00E0EA"

        "t" ->
            "#FFFFFF"

        _ ->
            ""


noteSvg : Note -> Svg msg
noteSvg note =
    let
        duration =
            note.durationType

        nWidth =
            noteWidth duration

        fillColor =
            noteColor duration

        opaque =
            if note.hz == 0.0 then
                "0.2"

            else
                "1"

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
        , opacity opaque
        , Svg.Attributes.filter "drop-shadow(1px 1px 2px #aaa)"
        ]
        []


getNoteDuration : String -> Float
getNoteDuration key =
    case String.toLower key of
        "w" ->
            4.0

        "f" ->
            2.0

        "q" ->
            1.0

        "e" ->
            0.5

        "s" ->
            0.25

        "t" ->
            0.125

        _ ->
            0.0


addNotes : List Note -> Note -> List Note
addNotes notes note =
    if note.duration > 0.1 then
        note :: notes

    else
        notes


noteHz : String -> Float
noteHz name =
    case String.toLower name of
        "c" ->
            65.405

        "c#" ->
            69.5

        "db" ->
            69.5

        "d" ->
            73.415

        "d#" ->
            78.0

        "eb" ->
            78.0

        "e" ->
            82.405

        "f" ->
            87.305

        "f#" ->
            92.5

        "gb" ->
            92.5

        "g" ->
            98.0

        "g#" ->
            104.0

        "ab" ->
            104.0

        "a" ->
            110.0

        "a#" ->
            116.5

        "bb" ->
            116.5

        "b" ->
            123.47

        "r" ->
            0.0

        _ ->
            0.0


buildNote : String -> ( Float, Float ) -> Bool -> Note
buildNote noteDuration pos isRest =
    let
        { name, octave } =
            noteNameAndOctaveByPos -230 pos

        noteDurationFloat =
            getNoteDuration noteDuration

        hz =
            if isRest == True then
                0.0

            else
                noteHz name
    in
    if noteDurationFloat > 0.1 then
        Note (Tuple.first pos) (Tuple.second pos) name octave noteDurationFloat noteDuration hz

    else
        Note (Tuple.first pos) (Tuple.second pos) "" 0 0.0 "t" 0.0



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
    if distanceToBase >= 210 then
        4

    else if (distanceToBase >= 140) && (distanceToBase < 210) then
        3

    else if (distanceToBase >= 70) && (distanceToBase < 140) then
        2

    else if (distanceToBase >= -10) && (distanceToBase < 70) then
        1

    else
        0


noteNameAndOctaveByPos : Int -> ( Float, Float ) -> { name : String, octave : Int }
noteNameAndOctaveByPos base pos =
    let
        yPos =
            round (Tuple.second pos)

        distanceToBase =
            negate (base - yPos)

        octave =
            octaveByPos distanceToBase

        name =
            nameByPos (remainderBy 70 distanceToBase)
    in
    { octave = octave, name = name }


draw : List Note -> List (Svg msg)
draw notes =
    let
        beatCount =
            List.foldr (+) 0 (List.map (\n -> n.duration) notes)

        measureCount =
            if beatCount > 0 then
                floor (beatCount / 4.0)

            else
                0

        yPos =
            0
    in
    [ staffCanvas measureCount notes yPos "v1" ]
