port module Main exposing (..)

import Browser
import Browser.Events as BE
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Input as Input
import Json.Decode as Decode
import Model exposing (Model)
import Movement as Move
import Notes as Notes
import Random



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
    ( Model
        ( 60.0, -90.0 )
        []
        "Normal"
        []
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyInput String


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type NoteRhythm
    = Whole
    | Half
    | Quarter
    | Eigth
    | Sixteenth


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


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


port playNote : String -> Cmd msg


port fed : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyInput key ->
            let
                newModel =
                    case model.mode of
                        "Normal" ->
                            Move.parseMovement model key

                        "Input" ->
                            Input.parseInput model key

                        _ ->
                            Move.parseMovement model key

                --cmd =
                --case note of
                --"" ->
                --Cmd.none
                --_ ->
                --let
                --a =
                --Debug.log "Snap!" note
                --in
                --playNote note
            in
            ( newModel
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyPress (Decode.map KeyInput keyDecoder)
        ]



-- VIEW


cursor : Model -> Html Msg
cursor model =
    let
        pos =
            model.cursorPos

        x =
            Tuple.first pos

        y =
            negate (Tuple.second pos)

        color =
            case model.mode of
                "Normal" ->
                    "blue"

                "Input" ->
                    "red"

                _ ->
                    "blue"
    in
    div
        [ style "background-color" color
        , style "height" "20px"
        , style "width" "10px"
        , style "position" "fixed"
        , style "top" (Debug.toString y ++ "px")
        , style "left" (Debug.toString x ++ "px")
        ]
        []


view : Model -> Html Msg
view model =
    div
        [ style "background-color" "#E2E3DE"
        , style "height" "1000px"
        , style "color" "white"
        ]
        [ cursor model
        , Notes.defFlugel model.notes
        , h1 [] [ Html.text (Debug.toString model.noteInput) ]

        --, h1 [] [ text (Debug.toString model.notes) ]
        ]



-- TODO
-- Layers for polyphony (switch layer with key press, not mouse)
-- ledger lines
-- measure lines (add with key press)
-- Web audio
-- Edit / Delete notes
