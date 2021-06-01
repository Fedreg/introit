port module Main exposing (..)

import Browser
import Browser.Events as BE
import Cursor as Cursor
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (Model, Note, Sequence)
import Notes as Notes



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ( 60.0, -90.0 ) []
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyInput String
    | PlayAllNotes


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


getDirection : String -> String
getDirection key =
    case key of
        "j" ->
            "Down"

        "k" ->
            "Up"

        "h" ->
            "Left"

        "l" ->
            "Right"

        _ ->
            ""


port playNote : Note -> Cmd msg


port playAll : Sequence -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayAllNotes ->
            ( model
            , playAll (Sequence (List.reverse model.notes) [] 300)
            )

        KeyInput key ->
            let
                dir =
                    getDirection key

                noteDuration =
                    Notes.getNoteDuration key

                pos =
                    Cursor.getNewCursorPos model.cursorPos dir

                noteWidth =
                    Notes.noteWidthFloat noteDuration

                newPos =
                    ( Tuple.first pos + noteWidth, Tuple.second pos )

                note =
                    Notes.buildNote noteDuration pos

                notes =
                    Notes.addNotes model.notes note

                cmd =
                    if note.duration > 0.1 then
                        let
                            newNote =
                                { note | duration = 0.5 }
                        in
                        playNote newNote

                    else
                        Cmd.none
            in
            ( { model | cursorPos = newPos, notes = notes }
            , cmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyPress (Decode.map KeyInput keyDecoder)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "background-color" "#E2E3DE"
        , style "height" "1000px"
        , style "color" "white"
        ]
        [ Cursor.cursor model.cursorPos
        , Notes.draw model.notes
        , button [ onClick PlayAllNotes ] [ text "Play All!" ]
        , h1 [] [ Html.text (Debug.toString model.cursorPos) ]

        --, h1 [] [ text (Debug.toString model.notes) ]
        ]



-- TODO
-- Layers for polyphony (switch layer with key press, not mouse)
-- ledger lines
-- measure lines (add with key press)
-- Edit / Delete notes
