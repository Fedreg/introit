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



-- measureGroup : Int -> List (Svg msg)
-- measureGroup count =
--     List.map measure <| [ ( 50, 60 ), ( 210, 60 ), ( 370, 60 ) ]


staffCanvas : List Note -> Svg msg
staffCanvas notes =
    svg
        [ width "100%"
        , height "90%"
        , y "200"
        ]
        (List.concat
            [ measure (Tuple.pair 50 60)
            , measure (Tuple.pair 210 60)
            , measure (Tuple.pair 370 60)
            , measure (Tuple.pair 530 60)
            , measure (Tuple.pair 690 60)
            , measure (Tuple.pair 850 60)
            , measure (Tuple.pair 1010 60)
            , measure (Tuple.pair 1170 60)
            , measure (Tuple.pair 1330 60)
            , measure (Tuple.pair 1490 60)

            -- , staffLineGroup 220
            -- , staffLineGroup 380
            -- , staffLineGroup 540
            , List.map noteSvg <| notes
            ]
        )


noteWidth : Float -> String
noteWidth noteDuration =
    if noteDuration > 3.9 && noteDuration < 8.0 then
        "160"

    else if noteDuration > 1.9 && noteDuration < 4.0 then
        "80"

    else if noteDuration > 0.9 && noteDuration < 2.0 then
        "40"

    else if noteDuration > 0.4 && noteDuration < 1.0 then
        "20"

    else if noteDuration > 0.24 && noteDuration < 0.5 then
        "10"

    else if noteDuration > 0.124 && noteDuration < 0.25 then
        "5"

    else
        "0"


noteWidthFloat : Float -> Float
noteWidthFloat noteDuration =
    Maybe.withDefault 160.0 <| String.toFloat <| noteWidth noteDuration


noteColor : Float -> String
noteColor noteDuration =
    if noteDuration > 3.9 && noteDuration < 8.0 then
        "#6AA4B0"

    else if noteDuration > 1.9 && noteDuration < 4.0 then
        "#62CCC0"

    else if noteDuration > 0.9 && noteDuration < 2.0 then
        "#2B4560"

    else if noteDuration > 0.4 && noteDuration < 1.0 then
        "#E34234"

    else if noteDuration > 0.24 && noteDuration < 0.5 then
        "#00E0EA"

    else if noteDuration > 0.124 && noteDuration < 0.25 then
        "#FFFFFF"

    else
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


buildNote : Float -> ( Float, Float ) -> Bool -> Note
buildNote noteDuration pos isRest =
    let
        { name, octave } =
            noteNameAndOctaveByPos -150 pos

        hz =
            if isRest == True then
                0.0

            else
                noteHz name
    in
    if noteDuration > 0.1 then
        Note (Tuple.first pos) (Tuple.second pos) name octave noteDuration hz

    else
        Note (Tuple.first pos) (Tuple.second pos) "" 0 0.0 0.0



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
