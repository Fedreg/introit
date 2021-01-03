module Input exposing (parseInput)

import Model exposing (Model, Note)
import Notes exposing (distanceToBase, noteWidth)


getNoteName : String -> String
getNoteName key =
    case String.toLower key of
        "a" ->
            "A"

        "b" ->
            "B"

        "c" ->
            "C"

        "d" ->
            "D"

        "e" ->
            "E"

        "f" ->
            "F"

        "g" ->
            "G"

        _ ->
            ""


getNoteOctave : String -> String
getNoteOctave key =
    case key of
        "1" ->
            "1"

        "2" ->
            "2"

        "3" ->
            "3"

        "4" ->
            "4"

        "5" ->
            "5"

        "6" ->
            "6"

        _ ->
            ""


getNoteAccidental : String -> String
getNoteAccidental key =
    case key of
        "#" ->
            "#"

        "b" ->
            "b"

        _ ->
            ""


getNoteDuration : String -> String
getNoteDuration key =
    case String.toLower key of
        "w" ->
            "W"

        "h" ->
            "H"

        "q" ->
            "Q"

        "e" ->
            "E"

        "s" ->
            "S"

        _ ->
            ""


appendOrReturn : List String -> String -> List String
appendOrReturn currentNoteInput attribute =
    case attribute of
        "" ->
            currentNoteInput

        _ ->
            List.append currentNoteInput [ attribute ]


buildNote : List String -> String -> List String
buildNote currentNoteInput key =
    case currentNoteInput of
        [] ->
            let
                name =
                    getNoteName key
            in
            appendOrReturn currentNoteInput name

        [ a ] ->
            let
                octave =
                    getNoteOctave key
            in
            appendOrReturn currentNoteInput octave

        [ a, b ] ->
            let
                duration =
                    getNoteDuration key
            in
            appendOrReturn currentNoteInput duration

        _ ->
            []



--currentNoteInput


parseInput : Model -> String -> Model
parseInput model key =
    let
        newMode =
            case key of
                "i" ->
                    "Input"

                "n" ->
                    "Normal"

                _ ->
                    "Input"

        newNoteInput =
            case key of
                "i" ->
                    []

                "n" ->
                    []

                _ ->
                    buildNote model.noteInput key

        newNotes =
            case newNoteInput of
                [ name, octave, duration ] ->
                    let
                        x =
                            Tuple.first model.cursorPos

                        y =
                            distanceToBase model.staffBase name octave

                        intOctave =
                            Maybe.withDefault 4 (String.toInt octave)
                    in
                    Note x y name intOctave duration :: model.notes

                _ ->
                    model.notes

        newCursorPos =
            case newNoteInput of
                [ name, octave, duration ] ->
                    let
                        width =
                            Maybe.withDefault 160.0 (String.toFloat (noteWidth duration))

                        x =
                            Tuple.first model.cursorPos + width

                        y =
                            distanceToBase model.staffBase name octave
                    in
                    ( x, y )

                _ ->
                    model.cursorPos
    in
    { model | mode = newMode, noteInput = newNoteInput, notes = newNotes, cursorPos = newCursorPos }
