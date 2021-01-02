module Input exposing (parseInput)

import Model exposing (Model)


getNoteName : String -> String
getNoteName key =
    let
        a =
            Debug.log "Pickles" (String.toLower key)
    in
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


buildNote : List String -> String -> List String
buildNote currentNoteInput key =
    let
        el =
            case List.length currentNoteInput of
                1 ->
                    getNoteName key

                2 ->
                    getNoteOctave key

                3 ->
                    getNoteDuration key

                _ ->
                    ""
    in
    List.append currentNoteInput [ el ]


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
            buildNote model.noteInput key
    in
    { model | mode = newMode, noteInput = newNoteInput }
