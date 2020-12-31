module Main exposing (..)

import Browser
import Browser.Events as BE
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Model exposing (Model)
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
    ( Model ( 50.0, -200.0 ) []
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


getNote : String -> String
getNote key =
    case key of
        "w" ->
            "W"

        "r" ->
            "H"

        "q" ->
            "Q"

        "e" ->
            "E"

        "s" ->
            "S"

        _ ->
            ""


moveConst : Float
moveConst =
    10.0


getNewCursorPos : ( Float, Float ) -> String -> ( Float, Float )
getNewCursorPos currentPos dir =
    let
        x =
            Tuple.first currentPos

        y =
            Tuple.second currentPos
    in
    case dir of
        "Left" ->
            ( Basics.max (x - moveConst) 0, y )

        "Right" ->
            ( x + moveConst, y )

        "Up" ->
            ( x, Basics.min (y + moveConst) 0 )

        "Down" ->
            ( x, y - moveConst )

        _ ->
            ( x, y )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyInput key ->
            let
                dir =
                    getDirection key

                note =
                    getNote key

                pos =
                    getNewCursorPos model.cursorPos dir

                notes =
                    addNotes model.notes note pos
            in
            ( { model | cursorPos = pos, notes = notes }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onKeyPress (Decode.map KeyInput keyDecoder)
        ]



-- VIEW


cursor : ( Float, Float ) -> Html Msg
cursor pos =
    let
        x =
            Tuple.first pos

        y =
            negate (Tuple.second pos)
    in
    div
        [ style "background-color" "red"
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
        [ style "background-color" "#111"
        , style "height" "1000px"
        , style "color" "white"
        ]
        [ cursor model.cursorPos
        , Notes.defFlugel model.notes

        --, h1 [] [ Html.text (Debug.toString model.cursorPos) ]
        --, h1 [] [ text (Debug.toString model.notes) ]
        ]
